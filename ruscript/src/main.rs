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

        let sum = i.call("add", vec![j]);

        match *sum {
            _Object::Int(ref intty) => { println!("unboxed: {:?}", intty.unbox()); }
            _ => {}
        }

        // gc::force_collect();
    }
    println!("out of scope");

    gc::force_collect();
}


