use error_stack::ResultExt;
use fyrc_utils::EntityId;
use rustc_hash::FxHashMap;

use crate::{
    block::{Block, BlockData},
    error::SsaResult,
    instr::InstrData,
};

enum BlockEnd {
    Jump(Block),
    ThenElse(Block, Block),
    Nothing,
}

impl crate::function::FunctionData {
    fn viz_block_label(
        &self,
        block: Block,
        block_data: &BlockData,
    ) -> SsaResult<(String, BlockEnd)> {
        let mut rows = vec![format!("<start>{}", block.to_string())];

        let mut var_map = FxHashMap::default();
        for (&var, block_map) in self.var_defs.iter() {
            for &val in block_map.values() {
                var_map.insert(val, var);
            }
        }

        for phi in block_data.phis.values() {
            let phi_data = self.get_phi_data(*phi)?;
            let value_data = self
                .get_value(phi_data.value)
                .attach_printable("while constructing phi row")?;
            let var_data = self
                .get_var(phi_data.var)
                .attach_printable("while constructing phi row")?;
            let args: Vec<String> = phi_data
                .args
                .iter()
                .map(|(b, v)| format!("{}:B{}", v, b.get_id()))
                .collect();

            rows.push(format!(
                "{}:{}({}) = Φ({})",
                phi_data.value,
                value_data.value_type,
                var_data.name,
                args.join(","),
            ));
        }

        for instr in block_data.instrs.iter().copied() {
            let result = self.results.get(&instr);
            let mut row = String::new();

            if let Some(resval) = result {
                let value_data = self
                    .get_value(*resval)
                    .attach_printable("while constructing phi row")?;

                row.push_str(&format!("{}:{}", *resval, value_data.value_type));

                if let Some(&var_assign) = var_map.get(resval) {
                    let var_data = self.get_var(var_assign)?;
                    row.push_str(&format!("({})", var_data.name));
                }

                row.push_str(" = ");
            }

            let instr_data = self.get_instr(instr)?;
            let args: Vec<String> = instr_data
                .get_args()
                .iter()
                .map(|kind| kind.to_string())
                .collect();

            row.push_str(&format!("{}({})", instr_data.get_name(), args.join(",")));
            rows.push(row);
        }

        let block_end = block_data
            .exit
            .and_then(|instr| self.get_instr(instr).ok())
            .map_or(BlockEnd::Nothing, |instr_data| match instr_data {
                InstrData::Branch(br) => BlockEnd::ThenElse(br.then_block, br.else_block),
                InstrData::Jump(jmp) => BlockEnd::Jump(jmp.dest),
                _ => BlockEnd::Nothing,
            });

        match block_end {
            BlockEnd::ThenElse(_, _) => rows.push("{<then>then|<else>else}".to_string()),
            BlockEnd::Jump(_) => rows.push("<jump>jump".to_string()),
            BlockEnd::Nothing => {}
        }

        let label = format!("\"{{{}}}\"", rows.join("|"));

        Ok((format!("{block} [label={label}];"), block_end))
    }

    pub fn viz(&self) -> SsaResult<String> {
        let mut nodes = Vec::<String>::with_capacity(self.blocks.len());
        let mut edges = Vec::<String>::new();

        for (block, block_data) in self.blocks.iter() {
            let (node_def, branch) = self.viz_block_label(block, block_data)?;
            nodes.push(node_def);
            match branch {
                BlockEnd::ThenElse(thenb, elseb) => {
                    edges.push(format!("{block}:then -> {thenb}:start:n;"));
                    edges.push(format!("{block}:else -> {elseb}:start:n;"));
                }

                BlockEnd::Jump(jmpb) => {
                    edges.push(format!("{block}:jump -> {jmpb}:start:n;"));
                }

                BlockEnd::Nothing => {}
            }
        }

        let mut stmts = vec!["node [shape=record];".to_string()];
        stmts.append(&mut nodes);
        stmts.append(&mut edges);

        Ok(format!(
            "digraph function {{\n    {}\n}}",
            stmts.join("\n    ")
        ))
    }
}
