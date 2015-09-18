#![feature(box_syntax)]

extern crate ruscript;
extern crate gc;
use std::collections::HashMap;
use ruscript::object::*;
use ruscript::interpreter::*;
use gc::Gc;

fn main() {
    let mut env = Environment {
        objects : &mut HashMap::new()
    };

    println!("{:?}", env.objects.len());

    let prim = Object::PrimObj(PrimitiveType::PrimInt(1));
    let prim2 = Object::PrimObj(PrimitiveType::PrimInt(1));

    env.objects.insert("hello".to_string(), Gc::new(prim));

    println!("{:?}", env.objects.len());

    let templ = Template {
        name : "test".to_string(),
        methods : HashMap::new(),
    };

    let comp = Complex {
        template : box templ,
        attributes : vec![box prim2]
    };

    env.objects.insert("world".to_string(), Gc::new(Object::CompObj(comp)));

    println!("{:?}", env.objects.len());

    let st = exampleStatement(); // x := 1

    interprete(&st, &mut env);

    println!("{:?}", env.objects.len());    
}
