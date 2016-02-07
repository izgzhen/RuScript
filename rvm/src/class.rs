//! 
//! Class structure
//!
//! The class is mostly used immutable, i.e., once parsed
//! from the bytecode stream, it will never change. The
//! inheritance is maintained in this structure as well.
//! Code resue is down by querying bottom up, which might
//! not be very efficient, but simpler

use std::collections::HashMap;
use bytecode::ByteCode;
use bytecode::ByteCode::*;
use function::*;
use super::*;
use env::Env;

pub struct Class {
    pub father  : Option<usize>,
    pub methods : HashMap<String, Function>,
    pub attrs   : Vec<String>,
}

impl Class {
    /// Get a function structure of method by name
    pub fn get_method<'a>(&'a self, name: &String, env: &'a Env) -> Option<&'a Function> {
        match self.methods.get(name) {
            Some(method) => Some(method),
            None => {
                match self.father {
                    Some(idx) => env.classes[idx].get_method(name, env),
                    None => panic!("Can' find method {:?}", name),
                }
            }
        }
    }
}

/// Parse class from bytecode stream
pub fn parse_class(code: &Vec<ByteCode>, n_attrs: usize,
                   n_methods: usize, father_idx: Integer, pc: &mut usize) -> Class {

    let mut attrs = Vec::new();
    let mut methods = HashMap::new();

    // Collect the attributes' names
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

        // Collect the method name
        match code[*pc] {
            PUSHSTR(ref s) => { methods.insert(s.clone(), function); },
            _ => panic!("End of method function must be followed with PUSHSTR"),
        }

        *pc = *pc + 1;
    }

    // father_idx < 0 means no inheritance
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
