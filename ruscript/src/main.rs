 #![feature(custom_derive)]
 #![feature(box_syntax)]

extern crate ruscript;
extern crate gc;

use ruscript::object::*;
use ruscript::primty::*;
use ruscript::framety::*;
use ruscript::classty::*;
use gc::*;
use ruscript::stackcode::SCode::*;
use ruscript::Object;

fn main() {
    {

        let i = Int_ty::new(10);
        let j = Int_ty::new(20);

        let cb = Box::new(vec![PUSH(0), PUSH(1), ADD, RET]);

        let mtd = Method_ty {
            name  : "adder".to_string(),
            frame : Frame_ty::new(cb, Box::new(vec![])),
        };

        let cls = Class_ty::new("example", vec![mtd], vec![]); // No locals

        let child_obj = __new__(&cls);

        let globals = Box::new(vec![child_obj]);

        let cb2 = Box::new(vec![PUSH(0), PUSH(1), CALL(0, "adder".to_string(), 2), RET]);

        let mtd = Method_ty {
            name  : "bridge".to_string(),
            frame : Frame_ty::new(cb2, globals),
        };

        let cls2 = Class_ty::new("example2", vec![mtd], vec![]);

        let child_obj2 = __new__(&cls2);

        let ret = child_obj2.call("bridge", vec![i, j]);

        match *ret {
            _Object::Int(ref intty) => { println!("unboxed: {:?}", intty.unbox()); },
            _ => {}
        }
    }
    println!("out of scope");

    gc::force_collect();
}


