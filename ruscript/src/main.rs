 #![feature(custom_derive)]
 #![feature(box_syntax)]

extern crate ruscript;
extern crate gc;

use std::collections::HashMap;
use ruscript::object::*;
use ruscript::interpreter::*;
use gc::*;

fn main() {
    {
        let mut env = Environment {
            objects : &mut HashMap::new()
        };

        let prim = Object::PrimObj(PrimitiveType::PrimInt(1));
        let prim2 = Object::PrimObj(PrimitiveType::PrimInt(2));
        env.objects.insert("a".to_string(), Gc::new(prim));
        env.objects.insert("a".to_string(), Gc::new(prim2)); // The first 1 is unaccessible, so it should be collected
    }

    gc::force_collect();
}


