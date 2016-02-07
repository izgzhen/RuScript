//! 
//! Runtime Environment
//!
//! The data structure and loader. The
//! environment is assumed to be immutable
//! 

use class::*;
use bytecode::ByteCode;
use deserialize::*;
use std::fs::File;
use std::io::Read;
use function::*;

pub struct Env {
    pub classes   : Vec<Class>,
    pub functions : Vec<Function>,
}

/// Load bytes from a file handler, deserialize, split top-level
/// statements from declarations
pub fn load(f: &mut File) -> (Env, Vec<ByteCode>, usize) {
    let mut classes   = vec![];
    let mut functions = vec![];
    let mut top_code  = vec![];
    let mut bytes: Vec<u8> = Vec::new();
    let top_n: usize;

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
            while pc < (code.len() - 1) {
                match code[pc] {
                    ByteCode::CLASS(nattrs, nmtds, father_idx) => {
                        pc = pc + 1;
                        let class = parse_class(&code, nattrs as usize, nmtds as usize,
                                                father_idx, &mut pc);
                        classes.push(class);
                    },
                    ByteCode::SFUNC => {
                        pc = pc + 1;
                        functions.push(parse_function(&code, &mut pc));
                    },
                    ref inst => {
                        top_code.push(inst.clone());
                        pc = pc + 1;
                    }
                }
            }

            match code[code.len() - 1] {
                ByteCode::PUSHINT(i) => top_n = i as usize,
                _ => panic!("The program doesn't end with top-level variables number")
            }
        },
        Err(e) => {
            panic!("reading bytes error: {}", e);
        }
    }

    (Env { classes : classes, functions : functions }, top_code, top_n)
}
