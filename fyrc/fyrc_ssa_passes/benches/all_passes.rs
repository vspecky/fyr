#![allow(unused)]
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fyrc_ssa_passes::test_utils;

macro_rules! benchmark_passes {
    ($criterion:ident, $func:ident, [$($pass:ident),+]) => {
        $(
            $criterion.bench_function(&format!("Pass({})", stringify!($pass)), |b| {
                b.iter(|| {
                    test_utils::get_pass::<fyrc_ssa_passes::$pass>($func.clone());
                });
            });
        )+
    };
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let double = test_utils::get_function_with_nested_loops();

    benchmark_passes!(
        c,
        double,
        [
            CriticalEdgeRemoval,
            DfsTree,
            LoopNestingForest,
            LivenessAnalysis,
            InterferenceGraph,
            DominatorTree,
            CssaTranslation,
            DefUse,
            GlobalNextUse
        ]
    );
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
