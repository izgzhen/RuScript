#![crate_name = "ruscript"]

#![feature(plugin, custom_derive, custom_attribute, box_syntax, test)]
#![plugin(gc_plugin)]
extern crate gc;
extern crate test;

use gc::*;

pub type Integer = i32;

pub const GLOBAL_MAXSIZE : usize = 10;

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

pub fn run(class_def_code : &Vec<SCode>, top_level_code: &Vec<SCode>, top_level_objs: Vec<Gc<_Object>>) {
    let mut globals = Vec::with_capacity(self::GLOBAL_MAXSIZE);
    let void = primty::Int_ty::new(0);
    for _ in 0..self::GLOBAL_MAXSIZE {
        globals.push(void.clone());
    }

    let locals = Vec::new();
    let stack : GcCell<Vec<Gc<_Object>>> = GcCell::new(Vec::new());
    let env : Gc<Env> = Gc::new(Env {
            classes: load(class_def_code),
            global_objs: top_level_objs,
        });

    for inst in top_level_code {
        interprete(inst, &locals, &stack, &env, &mut globals);
    }
 
}

pub fn load(decl_code: &Vec<SCode>) -> Vec<Gc<Class_ty>> {
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

    classes
}


