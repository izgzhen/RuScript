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

    }
    println!("out of scope");

    gc::force_collect();
}


