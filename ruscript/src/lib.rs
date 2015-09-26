#![crate_name = "ruscript"]

#![feature(plugin, custom_derive, custom_attribute)]
#![feature(box_syntax)]
#![feature(test)]
#![plugin(gc_plugin)]
extern crate gc;
extern crate test;

use gc::*;

pub type ObjIdentTy = i32;
pub type ArgIdentTy = i8;

#[allow(non_camel_case_types)]
pub type int = i64;

pub mod object;
use object::*;

pub mod stackcode;

pub mod classty;
pub mod framety;
pub mod primty;
pub mod interprete;

#[cfg(test)]
mod benches;

use stackcode::*;
use stackcode::SCode::*;
use classty::*;
use interprete::*;

pub fn run(class_def_code : &Vec<SCode>, top_level_code: &Vec<SCode>) {
    let mut globals : Vec<Gc<_Object>> = Vec::new();
    let stack : GcCell<Vec<Gc<_Object>>> = GcCell::new(Vec::new());
    let env : Gc<Env> = load(class_def_code);

    for inst in top_level_code {
        interprete(inst, &globals, &stack, &env);
    }
 
}

pub fn load(decl_code: &Vec<SCode>) -> Gc<Env> {
    let mut pc : usize = 0;
    let mut classes = Vec::new();
    while pc < decl_code.len() {
        match decl_code[pc] {
            CLASS(x, y) => {
                let (class, pc_updated) = __class_decl__(decl_code, x as usize, y as usize, pc);
                pc = pc_updated;
                classes.push(class);
            },
            _ => { assert!(false) }
        }
    }

    Gc::new(Env {
        classes : classes,
        globals : Vec::new(),
    })
}


