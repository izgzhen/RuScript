/* 
 * Class structure and related
 *
 */

use std::collections::HashMap;
use bytecode::ByteCode;
use bytecode::ByteCode::*;

pub struct Class {
    pub methods : HashMap<String, Vec<ByteCode>>,
    pub attrs   : Vec<String>,
}

pub fn parse_class(code: &Vec<ByteCode>, n_attrs: usize, n_methods: usize, start: usize) -> (Class, usize) {
    let mut pc : usize = start;
    let mut attrs = Vec::new();
    let mut methods = HashMap::new();

    for _ in 0..n_attrs {
        match code[pc] {
            PUSHSTR(ref s) => {
                attrs.push(s.clone());
                pc = pc + 1;
            },
            _ => {
                pc = pc + 1;
                break;
            }
        }
    }

    let mut cb = Vec::new();

    for _ in 0..n_methods {

        loop {
            match code[pc] {
                EBODY => break,
                _ => {
                    cb.push(code[pc].clone());
                    pc = pc + 1;
                }
            }
        }

        pc = pc + 1;

        match code[pc] {
            PUSHSTR(ref s) => { methods.insert(s.clone(), cb); }
            _ => assert!(false, "EBODY must be followed with PUSHSTR"),
        }

        cb = Vec::new();
        pc = pc + 1;
    }

    (Class {
        methods : methods,
        attrs   : attrs,
    }
    , pc)
}
