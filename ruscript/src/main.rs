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
        gc::force_collect();
    }

    gc::force_collect();
}


