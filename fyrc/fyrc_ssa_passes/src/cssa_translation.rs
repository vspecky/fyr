use error_stack::{report, ResultExt};
use fyrc_ssa::{
    block::Block,
    function::FunctionData,
    instr::{CopyV, Instr, InstrData},
    value::{Value, ValueData, ValueKind},
};
use fyrc_utils::SimpleUnionFind;
use rustc_hash::{FxHashMap, FxHashSet};

use crate::error::PassResult;

#[derive(Debug, thiserror::Error)]
pub enum CssaTranslationError {
    #[error("there was an error while executing the algorithm")]
    AlgorithmError,
    #[error("failed a dependent pass for 'cssa_translation'")]
    DependentPassFailure,
    #[error("there was an error while interacting with the function data")]
    FunctionError,
    #[error("unable to find liveout set for block")]
    LiveOutSetNotFound,
    #[error("unable to find livein set for block")]
    LiveInSetNotFound,
}

type LabeledValue = (Option<Block>, Value);

struct CssaTranslationInner<'a> {
    func: &'a mut FunctionData,
    liveness: &'a mut crate::LivenessAnalysis,
    ig: &'a mut crate::InterferenceGraph,
    congruence: SimpleUnionFind<Value>,
}

impl<'a> CssaTranslationInner<'a> {
    fn new(
        func: &'a mut FunctionData,
        liveness: &'a mut crate::LivenessAnalysis,
        ig: &'a mut crate::InterferenceGraph,
    ) -> Self {
        Self {
            func,
            liveness,
            ig,
            congruence: SimpleUnionFind::new(),
        }
    }

    fn initialize_congruence_classes(&mut self) -> PassResult<(), CssaTranslationError> {
        for block in self.func.blocks.keys() {
            let block_phis = self
                .func
                .get_block_phis(block)
                .change_context(CssaTranslationError::FunctionError)?;

            for (_, phi) in block_phis {
                let phi_data = self
                    .func
                    .get_phi_data(phi)
                    .change_context(CssaTranslationError::FunctionError)?;
                self.congruence.add(phi_data.value);
                for &arg in phi_data.args.values() {
                    self.congruence.add(arg);
                }
            }
        }

        Ok(())
    }

    fn run_translation(&mut self) -> PassResult<(), CssaTranslationError> {
        self.initialize_congruence_classes()?;

        for block in self.func.blocks.keys() {
            let block_phis = self
                .func
                .get_block_phis(block)
                .change_context(CssaTranslationError::FunctionError)?;

            for (_, phi) in block_phis {
                let mut candidates = FxHashSet::<LabeledValue>::default();
                let mut unresolved_neighbors: FxHashMap<LabeledValue, FxHashSet<LabeledValue>> =
                    FxHashMap::default();
                let phi_data = self
                    .func
                    .get_phi_data_mut(phi)
                    .change_context(CssaTranslationError::FunctionError)?;
                let var = phi_data.var;
                let mut to_check: Vec<LabeledValue> =
                    phi_data.args.iter().map(|(&b, &v)| (Some(b), v)).collect();
                to_check.push((None, phi_data.value));

                let congsets: FxHashMap<LabeledValue, FxHashSet<Value>> = to_check
                    .iter()
                    .map(|(block, value)| {
                        ((*block, *value), self.congruence.get_set(*value).collect())
                    })
                    .collect();

                for (i, (b1, v1)) in to_check.iter().copied().enumerate() {
                    for (b2, v2) in to_check.iter().copied().skip(i + 1) {
                        if v1 == v2 {
                            continue;
                        }
                        let v1_congset = congsets
                            .get(&(b1, v1))
                            .ok_or_else(|| report!(CssaTranslationError::AlgorithmError))?;
                        let v2_congset = congsets
                            .get(&(b2, v2))
                            .ok_or_else(|| report!(CssaTranslationError::AlgorithmError))?;

                        let mut interference = false;
                        'outer: for iv1 in v1_congset.iter().copied() {
                            for iv2 in v2_congset.iter().copied() {
                                if iv1 == iv2 {
                                    continue;
                                }

                                if self.ig.interferes(iv1, iv2) {
                                    interference = true;
                                    break 'outer;
                                }
                            }
                        }

                        if !interference {
                            continue;
                        }

                        let b1_liveout = self
                            .liveness
                            .get_live_out(b1.unwrap_or(block))
                            .change_context(CssaTranslationError::LiveOutSetNotFound)?;
                        let b2_liveout = self
                            .liveness
                            .get_live_out(b2.unwrap_or(block))
                            .change_context(CssaTranslationError::LiveOutSetNotFound)?;

                        let v1_intersection = b1_liveout.intersection(v2_congset).next().is_some();
                        let v2_intersection = b2_liveout.intersection(v1_congset).next().is_some();

                        match (v1_intersection, v2_intersection) {
                            (true, false) => {
                                candidates.insert((b1, v1));
                            }

                            (false, true) => {
                                candidates.insert((b2, v2));
                            }

                            (true, true) => {
                                candidates.insert((b1, v1));
                                candidates.insert((b2, v2));
                            }

                            (false, false) => {
                                unresolved_neighbors
                                    .entry((b1, v1))
                                    .or_default()
                                    .insert((b2, v2));

                                unresolved_neighbors
                                    .entry((b2, v2))
                                    .or_default()
                                    .insert((b1, v1));
                            }
                        }
                    }
                }

                let mut unresolved_sorted = unresolved_neighbors.iter().collect::<Vec<_>>();
                unresolved_sorted.sort_by(|a, b| b.1.len().cmp(&a.1.len()));
                let mut resolved_labels = FxHashSet::default();
                let mut required_labels = FxHashSet::default();

                for (label, neighbors) in unresolved_sorted {
                    let has_unresolved_neighbor =
                        neighbors.difference(&resolved_labels).next().is_some();

                    if !has_unresolved_neighbor {
                        continue;
                    }

                    if !candidates.insert(*label) {
                        required_labels.insert(*label);
                    };
                    resolved_labels.insert(*label);
                }

                for resolved_label in &resolved_labels {
                    let neighbors = unresolved_neighbors
                        .get(resolved_label)
                        .ok_or_else(|| report!(CssaTranslationError::AlgorithmError))?;

                    let all_resolved = neighbors.difference(&resolved_labels).next().is_none();
                    if all_resolved && !required_labels.contains(resolved_label) {
                        candidates.remove(resolved_label);
                    }
                }

                let make_copy_instr =
                    |func_data: &mut FunctionData,
                     to_copy: Value|
                     -> PassResult<(Instr, Value), CssaTranslationError> {
                        let val_type = func_data
                            .get_value(to_copy)
                            .change_context(CssaTranslationError::FunctionError)?
                            .value_type;

                        let the_instr = func_data
                            .instrs
                            .insert(InstrData::CopyV(CopyV { val: to_copy }));

                        let copied_to = func_data.values.insert(ValueData {
                            value_type: val_type,
                            value_kind: ValueKind::InstrRes(the_instr),
                        });

                        func_data.results.insert(the_instr, copied_to);

                        Ok((the_instr, copied_to))
                    };

                self.ig.expand(candidates.len());

                for (maybe_block, value) in candidates {
                    if let Some(pred_block) = maybe_block {
                        let (the_instr, copied_to) = make_copy_instr(self.func, value)?;
                        self.func
                            .get_block_mut(pred_block)
                            .change_context(CssaTranslationError::FunctionError)?
                            .instrs
                            .push(the_instr);

                        self.func
                            .get_phi_data_mut(phi)
                            .change_context(CssaTranslationError::FunctionError)?
                            .args
                            .insert(pred_block, copied_to);

                        self.congruence.add(copied_to);

                        let all_succs = self
                            .func
                            .get_block_succs(pred_block)
                            .change_context(CssaTranslationError::FunctionError)?;

                        let mut original_in_live_in = false;
                        for &succ in &all_succs {
                            let in_live_in = self
                                .liveness
                                .get_live_in(succ)
                                .change_context(CssaTranslationError::LiveInSetNotFound)?
                                .contains(&value);

                            if in_live_in {
                                original_in_live_in = true;
                                break;
                            }
                        }

                        let mut used_in_phi = false;
                        for &succ in &all_succs {
                            let used = self
                                .func
                                .get_phi_by_block_var(succ, var)
                                .map_or(Ok(false), |phi| {
                                    self.func.get_phi_data(phi).map(|phi_data| {
                                        phi_data
                                            .args
                                            .get(&pred_block)
                                            .map_or(false, |phi_val| *phi_val == value)
                                    })
                                })
                                .change_context(CssaTranslationError::FunctionError)?;

                            if used {
                                used_in_phi = true;
                                break;
                            }
                        }

                        let live_out = self
                            .liveness
                            .get_live_out_mut(pred_block)
                            .change_context(CssaTranslationError::LiveOutSetNotFound)?;

                        live_out.insert(copied_to);

                        if !original_in_live_in && !used_in_phi {
                            live_out.remove(&value);
                        }

                        for &live_out_val in live_out.iter() {
                            self.ig.mark(copied_to, live_out_val);
                        }
                    } else {
                        let (the_instr, copied_to) = make_copy_instr(self.func, value)?;
                        self.func
                            .get_block_mut(block)
                            .change_context(CssaTranslationError::FunctionError)?
                            .instrs
                            .insert(0, the_instr);

                        self.func
                            .get_phi_data_mut(phi)
                            .change_context(CssaTranslationError::FunctionError)?
                            .value = copied_to;

                        self.congruence.add(copied_to);

                        let live_in = self
                            .liveness
                            .get_live_in_mut(block)
                            .change_context(CssaTranslationError::LiveInSetNotFound)?;

                        live_in.remove(&value);
                        live_in.insert(copied_to);

                        for &live_in_val in live_in.iter() {
                            self.ig.mark(copied_to, live_in_val);
                        }
                    }
                }

                let phi_data = self
                    .func
                    .get_phi_data(phi)
                    .change_context(CssaTranslationError::FunctionError)?;

                for &val in phi_data.args.values() {
                    self.congruence.union(phi_data.value, val);
                }
            }
        }
        Ok(())
    }
}

pub struct CssaTranslation;

impl crate::Pass for CssaTranslation {
    type Error = CssaTranslationError;

    fn name() -> String {
        "cssa_translation".to_string()
    }

    fn dependencies(mut tracker: crate::PassDepTracker) -> PassResult<(), Self::Error> {
        tracker
            .dependency::<crate::LivenessAnalysis>()
            .change_context(CssaTranslationError::DependentPassFailure)?;
        tracker
            .dependency::<crate::InterferenceGraph>()
            .change_context(CssaTranslationError::DependentPassFailure)?;

        Ok(())
    }

    fn execute(store: crate::PassStore) -> PassResult<Self, Self::Error> {
        let mut func = store.get_func_mut();
        let mut liveness = store
            .get_pass_mut::<crate::LivenessAnalysis>()
            .change_context(CssaTranslationError::DependentPassFailure)?;
        let mut ig = store
            .get_pass_mut::<crate::InterferenceGraph>()
            .change_context(CssaTranslationError::DependentPassFailure)?;

        let mut inner = CssaTranslationInner::new(&mut func, &mut liveness, &mut ig);
        inner.run_translation()?;

        Ok(Self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_utils;

    fn check_translation(func_data: FunctionData) {
        let ((liveness, ig), fd1) = test_utils::get_pass::<(
            crate::LivenessAnalysis,
            crate::InterferenceGraph,
        )>(func_data.clone());

        println!("BEFORE CSSA TRANSLATION\n");
        println!("\nBlock | LiveIn Set | LiveOut Set");
        for block in fd1.blocks.keys() {
            let live_in = liveness.get_live_in(block).expect("livein set");
            let live_out = liveness.get_live_out(block).expect("liveout set");

            println!("{block:?} | {live_in:?} | {live_out:?}");
        }

        println!("\nINTERFERENCE");
        for value in fd1.values.keys() {
            let set = ig.get_interference_set(value);
            println!("{value} -> {set:?}");
        }

        println!("\nFUNCTION");
        println!("{}", fd1.viz().expect("viz"));

        let ((liveness, ig, _), func_data) = test_utils::get_pass::<(
            crate::LivenessAnalysis,
            crate::InterferenceGraph,
            CssaTranslation,
        )>(func_data);

        println!("\nAFTER CSSA TRANSLATION");
        println!("\nBlock | LiveIn Set | LiveOut Set");
        for block in func_data.blocks.keys() {
            let live_in = liveness.get_live_in(block).expect("livein set");
            let live_out = liveness.get_live_out(block).expect("liveout set");

            println!("{block:?} | {live_in:?} | {live_out:?}");
        }

        println!("\nINTERFERENCE");
        for value in func_data.values.keys() {
            let set = ig.get_interference_set(value);
            println!("{value} -> {set:?}");
        }

        println!(
            "cssa translation paper example:\n{}",
            func_data.viz().expect("viz")
        );

        for block in func_data.blocks.keys() {
            let block_phis = func_data.get_block_phis(block).expect("block phis");

            for (_, phi) in block_phis {
                let phi_data = func_data.get_phi_data(phi).expect("phi data");

                let mut to_check: Vec<Value> = phi_data.args.iter().map(|(_, &v)| v).collect();
                to_check.push(phi_data.value);

                for (i, &v1) in to_check.iter().enumerate() {
                    for &v2 in to_check.iter().skip(i + 1) {
                        assert!(
                            !ig.interferes(v1, v2),
                            "interfering phi resources found after CSSA translation"
                        );
                    }
                }
            }
        }
    }

    #[test]
    fn test_paper_simple_example() {
        let func_data = fyrc_ssa_builder::build_function! {
            (#block finalb)
            (#block thenb)
            (#vdecl varx "x"::Int32)

            (x2 = const32 34)
            (c1 = const32 10)
            (x1 = add x2, c1)
            (#vdef varx x1)
            (cmps x2, x1)
            (brs LessThan, thenb, finalb)

            (#switch thenb)
            (#seal thenb)
            (#vdef varx x2)
            (jmp finalb)

            (#switch finalb)
            (#seal finalb)
            (#vuse varx x3)
            (ret Some(x3))
        };

        check_translation(func_data);
    }

    #[test]
    fn test_paper_complex_example() {
        let func_data = fyrc_ssa_builder::build_function! {
            (#block finalb)
            (#block jumpb)
            (#block headerb)
            (#vdecl varx "x1"::Int32)

            (x3 = const32 34)
            (c1 = const32 10)
            (x1 = add x3, c1)
            (#vdef varx x3)
            (cmps x3, x1)
            (brs LessThan, headerb, jumpb)

            (#switch jumpb)
            (#seal jumpb)
            (#vdef varx x3)
            (jmp finalb)

            (#switch headerb)
            (#seal headerb)
            (#block thenb)
            (#block elseb)
            (x2 = const32 50)
            (#vdef varx x2)
            (c3 = const32 60)
            (cmps x2, c3)
            (brs GreaterThan, thenb, elseb)

            (#switch thenb)
            (#seal thenb)
            (#vdef varx x2)
            (jmp finalb)

            (#switch elseb)
            (#seal elseb)
            (#vdef varx x1)
            (jmp finalb)

            (#switch finalb)
            (#seal finalb)
            (#vuse varx x0)
            (ret Some(x0))
        };

        check_translation(func_data);
    }
}
