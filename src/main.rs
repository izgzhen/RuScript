 #![feature(custom_derive)]
 #![feature(box_syntax, convert)]

extern crate ruscript;
extern crate gc;

use gc::*;
use ruscript::stackcode::*;
use std::fs::File;
use std::io::Read;
use std::env;

fn main() {
    if let Some(ref src) = env::args().nth(1) {
        let classes = vec![];
        let top_objs = vec![];
        let mut top_code = vec![];
        let mut f_opt = File::open(src);
        match f_opt {
            Ok(ref mut f) => {
                let mut bytes: Vec<u8> = Vec::new();
                match Read::read_to_end(f, &mut bytes) {
                    Ok(len) => {
                        let mut start_pos = 0;
                        loop {
                            let scode: SCode = deserialize(bytes.as_slice(), &mut start_pos);
                            top_code.push(scode);
                            if start_pos >= len {
                                break
                            }
                        }
                    },
                    Err(e) => {
                        assert!(false, "reading bytes {:?} error: {}", src, e);
                    }
                }
            },
            Err(_) => {}
        }
        ruscript::run(&classes, &top_code, top_objs);
    } else {
        println!("Usage: {} <source>", env::args().nth(0).unwrap());
    }

    gc::force_collect();
}


