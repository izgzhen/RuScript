 #![feature(custom_derive)]
 #![feature(box_syntax, convert)]

extern crate ruscript;
extern crate gc;

use ruscript::object::*;
use ruscript::primty::*;
use ruscript::framety::*;
use ruscript::classty::*;
use gc::*;
use ruscript::stackcode::SCode::*;
use ruscript::stackcode::*;
use std::fs::File;
use std::io::Read;

fn main() {
    {
        let classes = vec![];
        let top_objs = vec![];
        let mut top_code = vec![];
        let mut f_opt = File::open("compiler-hs/examples/demo.rusb");
        match f_opt {
            Ok(ref mut f) => {
                let mut bytes: Vec<u8> = Vec::new();
                Read::read_to_end(f, &mut bytes);
                let len = bytes.len();
                let mut start_pos = 0;
                loop {
                    let scode: SCode = deserialize(bytes.as_slice(), &mut start_pos);
                    top_code.push(scode);
                    if start_pos >= len {
                        break
                    }
                }
            },
            Err(_) => {}
        }

        ruscript::run(&classes, &top_code, top_objs);
    }

    gc::force_collect();
}


