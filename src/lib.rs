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
// use stackcode::SCode::*;
use classty::*;
use interprete::*;

pub fn run(classes: Vec<Gc<Class_ty>>, top_level_code: &Vec<SCode>) {
    let mut globals = Vec::with_capacity(self::GLOBAL_MAXSIZE);
    let mut scratch = Vec::with_capacity(self::GLOBAL_MAXSIZE);
    let void = primty::Int_ty::new(0);
    for _ in 0..self::GLOBAL_MAXSIZE {
        globals.push(void.clone());
        scratch.push(void.clone());
    }

    let stack : GcCell<Vec<Gc<_Object>>> = GcCell::new(Vec::new());
    let env : Gc<Env> = Gc::new(Env {
            classes: classes,
        });

    for inst in top_level_code {
        match inst {
            &SCode::RET => { },
            &SCode::CALL(recv, ref method, narg) => {
                let ref obj = globals[recv as usize].clone();

                let mut params = Vec::new();

                for _ in 0..narg {
                    let x = stack.borrow_mut().pop().unwrap();
                    params.push(x.clone());
                }
                stack.borrow_mut().push(obj.call(method, params, &env, &mut globals));
            },
            inst => interprete(inst, &mut scratch, &stack, &env, &mut globals)
        }
    }
 
}

