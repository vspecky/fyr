use error_stack::ResultExt;
use fxhash::FxHashMap;
use fyrc_utils::EntityId;

use crate::{
    block::{Block, BlockData},
    error::SsaResult,
    instr::{ArgKind, InstrData},
    phi::PhiData,
    value::{Value, ValueData},
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

        let get_phi_symbol = |data: &PhiData| if data.is_mem { "φ" } else { "Φ" };
        let get_value_repr = |value: Value, data: &ValueData| {
            if data.is_mem {
                format!("&{}", value.get_id())
            } else {
                format!("${}", value.get_id())
            }
        };

        for phi in block_data.var_phi_map.values() {
            let phi_data = self.get_phi_data(*phi)?;
            let value_data = self
                .get_value(phi_data.value)
                .attach_printable("while constructing phi row")?;
            let var_data = self
                .get_var(phi_data.var)
                .attach_printable("while constructing phi row")?;

            let mut args = Vec::new();
            for (&arg_block, &arg_val) in phi_data.args.iter() {
                let arg_val_data = self
                    .get_value(arg_val)
                    .attach_printable("while constructing phi row")?;

                args.push(format!(
                    "{}:B{}",
                    get_value_repr(arg_val, arg_val_data),
                    arg_block.get_id()
                ));
            }

            rows.push(format!(
                "{}:{}({}) = {}({})",
                get_value_repr(phi_data.value, value_data),
                value_data.value_type,
                var_data.name,
                get_phi_symbol(phi_data),
                args.join(","),
            ));
        }

        for instr in block_data.iter_instr() {
            let result = self.results.get(&instr);
            // let mut row = String::new();
            let mut row = format!("{{I{}:|", instr.get_id());

            if let Some(&resval) = result {
                let value_data = self
                    .get_value(resval)
                    .attach_printable("while constructing phi row")?;

                row.push_str(&format!(
                    "{}:{}",
                    get_value_repr(resval, value_data),
                    value_data.value_type
                ));

                if let Some(&var_assign) = var_map.get(&resval) {
                    let var_data = self.get_var(var_assign)?;
                    row.push_str(&format!("({})", var_data.name));
                }

                row.push_str(" = ");
            }

            let instr_data = self.get_instr(instr)?;
            let mut args = Vec::new();
            for arg_kind in instr_data.get_args().iter() {
                let repr = match arg_kind {
                    ArgKind::Value(val) => {
                        let value_data = self
                            .get_value(*val)
                            .attach_printable("when constructing arguments")?;

                        get_value_repr(*val, value_data)
                    }

                    ArgKind::NamedValue(_, val) => {
                        let value_data = self
                            .get_value(*val)
                            .attach_printable("when constructing arguments")?;

                        get_value_repr(*val, value_data)
                    }

                    rest => rest.to_string(),
                };

                args.push(repr);
            }

            row.push_str(&format!("{}({})}}", instr_data.get_name(), args.join(",")));
            rows.push(row);
        }

        let block_end =
            self.get_instr(block_data.exit)
                .ok()
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
