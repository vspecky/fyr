#![allow(unused)]
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fyrc_ssa::function::FunctionData;
use fyrc_ssa_builder::ssa_dsl;
use fyrc_ssa_passes::*;

fn execute_passes(func: FunctionData) -> PassManager {
    let mut pass_manager = PassManager::new(func);

    macro_rules! ensure_passes {
        ($($pass:ty),+) => {
            $(
                pass_manager
                    .ensure_pass::<$pass>()
                    .expect("pass failed");
            )+
        }
    }

    ensure_passes!(
        GlobalNextUse,
        DefUse,
        DominatorTree,
        LivenessAnalysis,
        LoopNestingForest,
        DfsTree
    );

    pass_manager
}

pub fn regalloc_benchmarks(c: &mut Criterion) {
    let func = ssa_dsl! {(
        (let v0 10)
        (let v1 20)
        (let v2 (+ v0 v1))
        (while v1 v2
            ((let v5 20)
            (let v4 (+ v1 v2))
            (mut v1 (+ v4 v5))
            (let v6 (+ v0 v5))))
        (let v3 (+ v0 10))
        (ret v3)
    )};

    c.bench_function("Spilling", |b| {
        b.iter(|| {
            let pass_manager = execute_passes(func.clone());
            macro_rules! get_pass {
                ($pass_name:ty) => {
                    pass_manager
                        .get_pass::<$pass_name>()
                        .expect("pass not found")
                };
            }
            let mut func = pass_manager.get_func_mut();
            let gnu = get_pass!(GlobalNextUse);
            let liveness = get_pass!(LivenessAnalysis);
            let loop_forest = get_pass!(LoopNestingForest);
            let def_use = get_pass!(DefUse);
            let dfs_tree = get_pass!(DfsTree);
            let dom = get_pass!(DominatorTree);
            let max_regs = 3;

            let ctx = fyrc_regalloc::spilling::SpillProcessCtx::new(
                &mut func,
                &gnu,
                &liveness,
                &loop_forest,
                &def_use,
                &dfs_tree,
                &dom,
                max_regs,
            );

            fyrc_regalloc::spilling::perform_spilling(ctx).expect("spilling");
        });
    });

    c.bench_function("Coloring", |b| {
        b.iter(|| {
            let pass_manager = execute_passes(func.clone());
            fyrc_regalloc::perform_regalloc(&pass_manager).expect("regalloc");
        });
    });
}

criterion_group!(benches, regalloc_benchmarks);
criterion_main!(benches);
