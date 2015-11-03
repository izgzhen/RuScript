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

fn main() {
    {
        let env = Gc::new(Env {
                    classes: vec![],
                    global_objs: vec![],
                    });
        let mut globals = Vec::new();
        let void = Int_ty::new(0);

        for _ in 0..ruscript::GLOBAL_MAXSIZE {
            globals.push(void.clone());
        }

        let frm = Frame_ty::new(Box::new(vec![PUSH_INT(1), POPG(0), PUSHG(0), PRINT]), &env);

        let ret = frm.call("__run__", vec![], &env, &mut globals);

        match *ret {
            _Object::Int(ref intty) => { println!("ret unboxed: {:?}", intty.unbox()); },
            _ => {}
        }

    }
    println!("out of scope");

    gc::force_collect();
}


