extern crate gc;
use test::Bencher;
use object::*;

#[bench]
fn bench_allocation(b: &mut Bencher) {
    b.iter(|| {
        {
            let i = Int_ty::new(10);
            let i = Int_ty::new(10);
        }

        gc::force_collect()
    })
}