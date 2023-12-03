#![allow(unused)]
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use fyrc_ssa_builder::ssa_dsl;

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

    fyrc_regalloc::build_spilling_ctx!(func, ctx, 3);

    c.bench_function("Spilling", |b| {
        b.iter(|| {
            let func = func.clone();
            fyrc_regalloc::build_spilling_ctx!(func, ctx, 3);
            fyrc_regalloc::spilling::perform_spilling(&mut ctx).expect("spilling");
        });
    });
}

criterion_group!(benches, regalloc_benchmarks);
criterion_main!(benches);
