 #![feature(custom_derive)]
 #![feature(box_syntax, convert)]

extern crate ruscript;
extern crate gc;

use gc::*;
use ruscript::stackcode::*;
use ruscript::classty::*;
use std::fs::File;
use std::io::Read;
use std::env;

fn main() {
    if let Some(ref src) = env::args().nth(1) {
        let mut classes  = vec![];
        let mut top_code = vec![];

        let mut f_opt = File::open(src);
        match f_opt {
            Ok(ref mut f) => {
                let mut bytes: Vec<u8> = Vec::new();
                match Read::read_to_end(f, &mut bytes) {
                    Ok(len) => {
                        let mut start_pos = 0;
                        let mut code = vec![];
                        loop {
                            code.push(deserialize(bytes.as_slice(), &mut start_pos));
                            if start_pos >= len {
                                break;
                            }
                        }

                        let mut pc = 0;
                        while pc < code.len() {
                            match code[pc] {
                                SCode::CLASS(ref nattrs, ref nmtds) => {
                                    let (class_obj, new_pc) = __class_decl__(&code, *nattrs as usize, *nmtds as usize, pc + 1);
                                    classes.push(class_obj);
                                    pc = new_pc;
                                },
                                ref scode => {
                                    top_code.push(scode.clone());
                                    pc = pc + 1;
                                }
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
        ruscript::run(classes, &top_code);
    } else {
        println!("Usage: {} <source>", env::args().nth(0).unwrap());
    }

    gc::force_collect();
}


