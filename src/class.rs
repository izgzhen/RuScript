/* 
 * Class structure and related
 *
 */

use std::collections::HashMap;
use bytecode::ByteCode;
use bytecode::ByteCode::*;
use function::*;
use super::*;

pub struct Class {
    pub father  : Option<usize>,
    pub methods : HashMap<String, Function>,
    pub attrs   : Vec<String>,
}

pub fn parse_class(code: &Vec<ByteCode>, n_attrs: usize,
                   n_methods: usize, father_idx: Integer, pc: &mut usize) -> Class {

    let mut attrs = Vec::new();
    let mut methods = HashMap::new();

    for _ in 0..n_attrs {
        match code[*pc] {
            PUSHSTR(ref s) => {
                attrs.push(s.clone());
                *pc = *pc + 1;
            },
            _ => {
                *pc = *pc + 1;
                break;
            }
        }
    }

    for _ in 0..n_methods {
        let function = parse_function(code, pc);

        match code[*pc] {
            PUSHSTR(ref s) => { methods.insert(s.clone(), function); }
            _ => assert!(false, "EBODY must be followed with PUSHSTR"),
        }

        *pc = *pc + 1;
    }

    let mut father = None;
    if father_idx >= 0 {
        father = Some(father_idx as usize);
    }

    Class {
        father  : father,
        methods : methods,
        attrs   : attrs,
    }
}
