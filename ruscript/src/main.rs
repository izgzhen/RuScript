 #![feature(custom_derive)]
 #![feature(box_syntax)]

extern crate ruscript;
extern crate gc;

use std::collections::HashMap;
use ruscript::object::*;
use gc::*;

fn main() {
    {
        let i = Int_ty::new(10);
        let j = Int_ty::new(20);
        let n = Int_ty::new(0);

        // let sum = i.call("add", vec![j]);

        // match *sum {
        //     _Object::Int(ref intty) => { println!("unboxed: {:?}", intty.unbox()); }
        //     _ => {}
        // }

        let arr = Array_ty::new();
        arr.call("push", vec![i, j]);

        gc::force_collect();

        let ret = arr.call("at", vec![n]);
        match *ret {
            _Object::Int(ref intty) => { println!("unboxed: {:?}", intty.unbox()); }
            _ => {}
        }
    }
    println!("out of scope");

    gc::force_collect();
}


