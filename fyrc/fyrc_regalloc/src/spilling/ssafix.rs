use crate::spilling::{SpillError, SpillProcessCtx, SpillResult};
use error_stack::{report, ResultExt};
use fxhash::{FxHashMap, FxHashSet};
use fyrc_ssa::{
    function::FunctionData,
    phi::PhiData,
    value::{Value, ValueData, ValueKind},
    variable::Variable,
    Block,
};
use fyrc_ssa_passes::{
    def_use::{DefUse, DefUseType},
    dominator_tree::{DominatorTree, Level},
};

mod idomf {
    use super::*;
    use fyrc_utils::{DenseMap, EntityId};

    use crate::spilling::{SpillError, SpillResult};

    #[derive(Debug)]
    struct PiggyBank {
        level_nodes: DenseMap<Level, Vec<Block>>,
        current_level: Option<Level>,
    }

    impl PiggyBank {
        fn new(highest_level: Level) -> Self {
            Self {
                level_nodes: DenseMap::with_prefilled(highest_level.get_id() + 1),
                current_level: if highest_level == Level::ZERO {
                    None
                } else {
                    Some(Level::with_id(highest_level.get_id().saturating_sub(1)))
                },
            }
        }

        fn insert_node(&mut self, node: Block, level: Level) -> SpillResult<()> {
            if self.current_level.is_none() {
                return Ok(());
            }

            let nodes = self
                .level_nodes
                .get_mut(level)
                .ok_or_else(|| report!(SpillError::DominatorLevelNotFound))?;

            if !nodes.contains(&node) {
                nodes.push(node);
            }

            Ok(())
        }

        fn get_node(&mut self) -> SpillResult<Option<Block>> {
            if let Some(current_level) = self.current_level {
                let level_list = self
                    .level_nodes
                    .get_mut(current_level)
                    .ok_or_else(|| report!(SpillError::DominatorLevelNotFound))?;

                if let Some(node) = level_list.pop() {
                    Ok(Some(node))
                } else {
                    for level in current_level.get_id()..=0 {
                        let level = Level::with_id(level);
                        let level_list = self
                            .level_nodes
                            .get_mut(level)
                            .ok_or_else(|| report!(SpillError::DominatorLevelNotFound))?;

                        if let Some(node) = level_list.pop() {
                            self.current_level = Some(level);
                            return Ok(Some(node));
                        }
                    }

                    self.current_level = None;
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        }
    }

    struct NodeData {
        visited: bool,
        alpha: bool,
        in_phi: bool,
        level: Level,
    }

    #[derive(Debug, Clone, Copy)]
    pub(super) enum EdgeType {
        Dominator,
        Join,
    }

    type DjGraph = FxHashMap<Block, FxHashMap<Block, EdgeType>>;

    /// Takes the Function Data and Dominator Tree information and constructs the DJ-Graph
    pub(super) fn construct_dj_graph(
        func: &FunctionData,
        dom: &DominatorTree,
    ) -> SpillResult<DjGraph> {
        let mut dj_graph = DjGraph::default();

        // iterate over the dominator tree edges and add all the dominator edges to the DJ-Graph
        for (child, &parent) in dom.parent.iter() {
            if child == Block::START {
                continue;
            }

            dj_graph
                .entry(parent)
                .or_default()
                .insert(child, EdgeType::Dominator);
        }

        // iterate over the control flow graph and add all the join edges, consulting the strict
        // dominator set.
        for block in func.blocks.keys() {
            let preds = func
                .get_block_preds(block)
                .change_context(SpillError::FunctionError)?;

            let sdom_set = dom
                .sdom
                .get(block)
                .ok_or_else(|| report!(SpillError::StrictDomSetNotFound))?;

            for pred in preds {
                if !sdom_set.contains(&pred) {
                    dj_graph
                        .entry(pred)
                        .or_default()
                        .insert(block, EdgeType::Join);
                }
            }
        }

        Ok(dj_graph)
    }

    fn visit(
        node: Block,
        dom: &DominatorTree,
        piggy_bank: &mut PiggyBank,
        idf: &mut FxHashSet<Block>,
        nodes: &mut DenseMap<Block, NodeData>,
        dj_graph: &DjGraph,
        root_level: Level,
    ) -> SpillResult<()> {
        let maybe_successors = dj_graph.get(&node);

        if let Some(successors) = maybe_successors {
            for (&succ, &edge_type) in successors.iter() {
                let node_data = nodes
                    .get_mut(succ)
                    .ok_or_else(|| report!(SpillError::DJGraphNodeDataNotFound))?;

                if matches!(edge_type, EdgeType::Join) {
                    let succ_level = dom
                        .block_levels
                        .get(succ)
                        .copied()
                        .ok_or_else(|| report!(SpillError::DominatorLevelNotFound))?;

                    if succ_level > root_level || node_data.in_phi {
                        continue;
                    }

                    node_data.in_phi = true;
                    idf.insert(succ);

                    if !node_data.alpha {
                        piggy_bank.insert_node(succ, succ_level)?;
                    }
                } else {
                    if !node_data.visited {
                        node_data.visited = true;
                        visit(succ, dom, piggy_bank, idf, nodes, dj_graph, root_level)?;
                    }
                }
            }
        }

        Ok(())
    }

    pub(super) fn calculate_iter_dom_frontier(
        func: &FunctionData,
        dom: &DominatorTree,
        dj_graph: &DjGraph,
        def_blocks: FxHashSet<Block>,
    ) -> SpillResult<FxHashSet<Block>> {
        let mut piggy_bank = PiggyBank::new(Level::with_id(dom.level_blocks.len() - 1));
        let mut node_data: DenseMap<Block, NodeData> = DenseMap::with_capacity(func.blocks.len());

        for block in func.blocks.keys() {
            node_data.insert(NodeData {
                visited: false,
                alpha: false,
                in_phi: false,
                level: dom
                    .block_levels
                    .get(block)
                    .copied()
                    .ok_or_else(|| report!(SpillError::DominatorLevelNotFound))?,
            });
        }

        for &n_block in def_blocks.iter() {
            node_data
                .get_mut(n_block)
                .ok_or_else(|| report!(SpillError::DJGraphNodeDataNotFound))?
                .alpha = true;

            let n_level = dom
                .block_levels
                .get(n_block)
                .copied()
                .ok_or_else(|| report!(SpillError::DominatorLevelNotFound))?;

            piggy_bank.insert_node(n_block, n_level)?;
        }

        let mut idf = FxHashSet::default();
        while let Some(current_root) = piggy_bank.get_node()? {
            node_data
                .get_mut(current_root)
                .ok_or_else(|| report!(SpillError::DJGraphNodeDataNotFound))?
                .visited = true;

            let root_level = dom
                .block_levels
                .get(current_root)
                .copied()
                .ok_or_else(|| report!(SpillError::DominatorLevelNotFound))?;

            visit(
                current_root,
                dom,
                &mut piggy_bank,
                &mut idf,
                &mut node_data,
                &dj_graph,
                root_level,
            )?;
        }

        Ok(idf)
    }
}

fn find_def(
    ctx: &mut SpillProcessCtx<'_>,
    defs: &mut FxHashMap<DefUseType, Value>,
    dom_frontier: &FxHashSet<Block>,
    value: Value,
    block: Block,
    instr: Option<DefUseType>,
) -> SpillResult<Value> {
    let (block, instrs) = match instr {
        Some(DefUseType::Phi(_phi, pred)) => {
            let pred_data = ctx
                .func
                .get_block(pred)
                .change_context(SpillError::FunctionError)?;

            (pred, pred_data.iter_instr_rev().collect::<Vec<_>>())
        }

        Some(DefUseType::Instr(ins)) => {
            let block_data = ctx
                .func
                .get_block(block)
                .change_context(SpillError::FunctionError)?;

            (
                block,
                block_data
                    .iter_instr_rev()
                    .skip_while(|i| *i != ins)
                    .skip(1)
                    .collect(),
            )
        }

        None => {
            let block_data = ctx
                .func
                .get_block(block)
                .change_context(SpillError::FunctionError)?;

            (block, block_data.iter_instr_rev().collect())
        }
    };

    for instr in instrs {
        if let Some(&val) = defs.get(&DefUseType::Instr(instr)) {
            return Ok(val);
        }
    }

    let block_data = ctx
        .func
        .get_block(block)
        .change_context(SpillError::FunctionError)?;

    for &phi in &block_data.phis {
        if let Some(&val) = defs.get(&DefUseType::Phi(phi, block)) {
            return Ok(val);
        }
    }

    if let &[single_pred] = &block_data.predecessors[..] {
        return find_def(ctx, defs, dom_frontier, value, single_pred, None);
    }

    if dom_frontier.contains(&block) {
        let entry_reg_set = ctx
            .entry_reg_sets
            .get(&block)
            .ok_or_else(|| report!(SpillError::BlockEntryRegSetMissing))?;

        let phi = ctx.func.phis.next_key();
        let phi_val = ctx.func.values.next_key();
        let value_type = ctx
            .func
            .get_value(value)
            .change_context(SpillError::FunctionError)?
            .value_type;

        ctx.func.phis.insert(PhiData {
            block,
            var: Variable::NONE,
            value: phi_val,
            args: FxHashMap::default(),
            is_mem: !entry_reg_set.contains(&value),
        });

        ctx.func.values.insert(ValueData {
            value_type,
            value_kind: if !entry_reg_set.contains(&value) {
                ValueKind::MemPhi(phi)
            } else {
                ValueKind::Phi(phi)
            },
            is_mem: !entry_reg_set.contains(&value),
        });

        defs.insert(DefUseType::Phi(phi, block), phi_val);
        let mut args = FxHashMap::default();
        let preds = ctx
            .func
            .get_block_preds(block)
            .change_context(SpillError::FunctionError)?;

        for pred in preds {
            let arg_val = find_def(ctx, defs, dom_frontier, value, pred, None)?;
            args.insert(pred, arg_val);
        }

        ctx.func
            .get_phi_data_mut(phi)
            .change_context(SpillError::FunctionError)?
            .args = args;

        return Ok(phi_val);
    }

    if block == Block::START {
        return Err(report!(SpillError::ValueDefNotFound));
    }

    let &dom_parent = ctx
        .dom
        .parent
        .get(block)
        .ok_or_else(|| report!(SpillError::DominatorParentNotFound))?;

    find_def(ctx, defs, dom_frontier, value, dom_parent, None)
}

pub(super) fn fix_ssa(ctx: &mut SpillProcessCtx<'_>) -> SpillResult<()> {
    let new_def_values = std::mem::take(&mut ctx.inserted_instrs);

    let dj_graph = idomf::construct_dj_graph(ctx.func, ctx.dom)?;
    for (value, new_defs) in new_def_values {
        let def_uses = ctx
            .def_use
            .get_def_use(value)
            .ok_or_else(|| report!(SpillError::DefUseNotFound))?;

        let mut def_blocks = new_defs
            .iter()
            .map(|(_, block)| *block)
            .collect::<FxHashSet<_>>();
        def_blocks.insert(def_uses.def_block);

        println!("def blocks: {:?}", def_blocks);
        let dom_frontier =
            idomf::calculate_iter_dom_frontier(ctx.func, ctx.dom, &dj_graph, def_blocks)?;
        println!("dom_frontier: {dom_frontier:?}");

        let mut total_defs = FxHashMap::default();
        for (instr, _) in &new_defs {
            let def = ctx
                .func
                .get_instr_def(*instr)
                .ok_or_else(|| report!(SpillError::ValueDefNotFound))?;

            total_defs.insert(DefUseType::Instr(*instr), def);
        }
        total_defs.insert(def_uses.def_type, value);

        let mut new_defs = new_defs
            .iter()
            .cloned()
            .map(|(i, b)| (DefUseType::Instr(i), b))
            .collect::<Vec<_>>();
        new_defs.extend_from_slice(&def_uses.uses);

        for (use_type, block) in new_defs {
            let new_def = find_def(
                ctx,
                &mut total_defs,
                &dom_frontier,
                value,
                block,
                Some(use_type),
            )?;

            match use_type {
                DefUseType::Phi(phi, phi_block) => {
                    let phi_data = ctx
                        .func
                        .get_phi_data_mut(phi)
                        .change_context(SpillError::FunctionError)?;

                    phi_data.args.insert(phi_block, new_def);
                }

                DefUseType::Instr(i) => {
                    let instr_data = ctx
                        .func
                        .get_instr_mut(i)
                        .change_context(SpillError::FunctionError)?;

                    instr_data.rewrite_value(value, new_def);
                }
            }
        }
    }

    Ok(())
}
