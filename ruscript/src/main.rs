extern crate ruscript;
extern crate gc;
use std::collections::HashMap;
use ruscript::object::*;
use gc::Gc;

fn main() {
    let env = Environment {
        primitives : &mut HashMap::new()
    };

    println!("{:?}", env.primitives.len());

    env.primitives.insert("hello".to_string(), Gc::new(Object::PrimObj(PrimitiveType::PrimInt(1))));

    println!("{:?}", env.primitives.len());
}