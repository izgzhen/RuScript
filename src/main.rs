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
        let classes = vec![];
        let top_objs = vec![];
        let top_code = vec![PUSH_INT(1), POPG(0), PUSHG(0), PRINT];

        ruscript::run(&classes, &top_code, top_objs);

    }
    println!("out of scope");

    gc::force_collect();
}


