 #![feature(custom_derive)]
 #![feature(box_syntax)]

extern crate ruscript;
extern crate gc;

use ruscript::object::*;
use gc::*;
use ruscript::object::OpCode::*;

fn main() {
    {
        let i = Int_ty::new(10);
        let j = Int_ty::new(20);

        let cb = Box::new(vec![PUSH(0), PUSH(1), ADD, RET]);
        let globals = Box::new(vec![]);

        let frame = Frame_ty::new(cb, globals);
        let ret = frame.call("__run__", vec![i, j]);
        match *ret {
            _Object::Int(ref intty) => { println!("unboxed: {:?}", intty.unbox()); }
            _ => {}
        }
    }
    println!("out of scope");

    gc::force_collect();
}


