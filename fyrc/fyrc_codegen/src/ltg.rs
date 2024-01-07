//! Location Transfer Graph module

use std::fmt;

use fxhash::FxHashMap;
use fyrc_machinst::types::Register;
use fyrc_regalloc::coloring::SpillSlot;

/// Represents a value location (Register, Spill Slot or Scratch Register)
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Location {
    Reg(Register),
    Mem(SpillSlot),
    Scratch,
}

impl From<Register> for Location {
    fn from(value: Register) -> Self {
        Self::Reg(value)
    }
}

impl From<SpillSlot> for Location {
    fn from(value: SpillSlot) -> Self {
        Self::Mem(value)
    }
}

impl fmt::Display for Location {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reg(reg) => write!(f, "r{}", reg.reg_id()),
            Self::Mem(ss) => write!(f, "m{}", ss.slot_id()),
            Self::Scratch => write!(f, "tmp"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Transfer {
    from: Location,
    to: Location,
}

impl fmt::Display for Transfer {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} := {}", self.to, self.from)
    }
}

/// Data Structure to Construct, Represent and Solve a Location Transfer Graph
#[derive(Debug)]
pub struct LocationTransferGraph {
    src: Vec<Location>,
    dest: Vec<Location>,
    preds: FxHashMap<Location, Location>,
}

impl LocationTransferGraph {
    pub fn new() -> Self {
        Self {
            src: Vec::new(),
            dest: Vec::new(),
            preds: FxHashMap::default(),
        }
    }

    /// Add an edge to the LTG. The in-degree of every node in the LTG can be a maximum of 2.
    /// If the insertion of an edge breaks this invariant for a node, the edge is not added
    /// and the function will return false. If the invariant still holds with the new edge,
    /// it is included in the LTG and the function returns true.
    pub fn add_edge<F, T>(&mut self, from: F, to: T) -> bool
    where
        F: Into<Location>,
        T: Into<Location>,
    {
        let from = from.into();
        let to = to.into();

        // In case of self-edges, the destination and source location are the same, i.e. in a way,
        // the edge is already resolved, so we do not add it to the graph, but return true since it
        // is resolved
        if from == to {
            return true;
        }

        if let Some(&pred) = self.preds.get(&to) {
            return pred == from;
        }

        self.src.push(from);
        self.dest.push(to);
        self.preds.insert(to, from);
        true
    }

    /// Constructs a GraphViz DiGraph of the LTG in the DOT DSL.
    pub fn viz(&self) -> String {
        let mut lines = Vec::with_capacity(self.preds.len());
        for (to, from) in &self.preds {
            lines.push(format!("    {from} -> {to};"));
        }

        format!("digraph ltg {{\n{}\n}}\n", lines.join("\n"))
    }

    pub fn solve(mut self) -> Vec<Transfer> {
        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum EdgeStatus {
            ToMove,
            BeingMoved,
            Moved,
        }
        use EdgeStatus::*;

        #[derive(Debug, Clone, Copy, PartialEq, Eq)]
        enum EdgeProcessingStatus {
            Unprocessed,
            Processed,
        }
        use EdgeProcessingStatus::*;

        let mut transfers = Vec::new();
        let mut status = vec![ToMove; self.src.len()];

        for iter_idx in 0..self.src.len() {
            if !matches!(status[iter_idx], ToMove) {
                continue;
            }

            let mut stack = vec![(iter_idx, Unprocessed)];
            while let Some((i, my_status)) = stack.pop() {
                if my_status == Processed {
                    transfers.push(Transfer {
                        from: self.src[i],
                        to: self.dest[i],
                    });
                    status[i] = Moved;

                    continue;
                }

                status[i] = BeingMoved;
                stack.push((i, Processed));

                for j in 0..self.src.len() {
                    if self.dest[i] != self.src[j] {
                        continue;
                    }

                    if status[j] == ToMove {
                        stack.push((j, Unprocessed));
                    } else if status[j] == BeingMoved {
                        transfers.push(Transfer {
                            to: Location::Scratch,
                            from: self.src[j],
                        });
                        self.src[j] = Location::Scratch;
                    }
                }
            }
        }

        transfers
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! make_ltg {
        (@loc #$reg_no:literal) => {
            Register($reg_no)
        };

        (@loc &$spill_slot:literal) => {
            SpillSlot($spill_slot)
        };

        (@edge($ltg:ident) $from_sym:tt $from_id:literal -> $to_sym:tt $to_id:literal; $($($rest:tt)+)?) => {
            $ltg.add_edge(make_ltg!(@loc $from_sym $from_id), make_ltg!(@loc $to_sym $to_id));
            $(make_ltg!(@edge($ltg) $($rest)+))?
        };

        ($($graph:tt)+) => {{
            let mut ltg = LocationTransferGraph::new();
            make_ltg!(@edge(ltg) $($graph)+);
            ltg
        }};
    }

    #[test]
    fn test_blade_ltg() {
        let ltg = make_ltg! {
            #0 -> &1;
            &1 -> #2;
            #2 -> &2;
            #2 -> &3;
        };

        println!("The Ltg:\n{}", ltg.viz());
        let result = ltg.solve();

        println!("The Solution:\n");
        for t in result {
            println!("{t}");
        }
    }

    #[test]
    fn test_windmill_ltg() {
        let ltg = make_ltg! {
            &2 -> #3;
            &2 -> #4;
            &2 -> &1;
            &1 -> #5;
            #5 -> &3;
            &1 -> #2;
            #2 -> #1;
            #1 -> &2;
        };

        println!("The Ltg:\n{}", ltg.viz());
        let result = ltg.solve();

        println!("The Solution:\n");
        for t in result {
            println!("{t}");
        }
    }
}
