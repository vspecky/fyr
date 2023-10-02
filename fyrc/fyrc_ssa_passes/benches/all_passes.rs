#![allow(unused)]
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fyrc_ssa_passes::test_utils;

pub fn criterion_benchmark(c: &mut Criterion) {
    let double = test_utils::get_function_with_nested_loops();
    c.bench_function("Without Test Function Construction", |b| {
        b.iter(|| {
            test_utils::get_pass::<(fyrc_ssa_passes::CssaTranslation, fyrc_ssa_passes::DominatorTree)>(double.clone());
        });
    });

    c.bench_function("With Test Function Construction", |b| {
        b.iter(|| {
            let double = test_utils::get_function_with_nested_loops();
            test_utils::get_pass::<(fyrc_ssa_passes::CssaTranslation, fyrc_ssa_passes::DominatorTree)>(double);
        });
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
