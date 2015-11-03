use gc::*;
use framety::*;
use super::*;
use super::object::*;
use super::object::_Object::*;
use super::stackcode::*;
use super::stackcode::SCode::*;

use std::cell::*;

///////////////// Class //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Method_ty {
    pub _name : Gc<String>,
    pub _code : Vec<SCode>,
}

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Class_ty {
    // name    : Gc<String>,
    methods : Vec<Method_ty>,
    attrs   : Vec<Gc<String>>,
}

impl Object for Class_ty {
    #[allow(unused_variables)]
    fn call(&self, name: &str, args: Vec<Gc<_Object>>, env: &Gc<Env>, globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            m => {
                println!("no such method {:?}", m);
                Gc::new(Non)
            }
        }
    }

    fn tyof(&self) -> &str { "<class>" }
}


///////////////// Instance //////////////////


#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Instance_ty{
    // env     : Gc<Env>,
    parent  : Gc<Class_ty>,
    locals  : Vec<Gc<_Object>>,
}

impl Object for Instance_ty {
    fn call(&self, name: &str, args: Vec<Gc<_Object>>, env: &Gc<Env>, globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {        
        for m in self.parent.methods.iter() {
            if (*m._name) == name.to_string() {
                let frame = Frame_ty::new(Box::new(m._code.clone()), env);
                return frame.call("__run__", args.clone(), env, globals);
            }
        }

        println!("no such method {:?}", name);
        Gc::new(Non)
    }

    fn tyof(&self) -> &str {
        "<instance>"
    }
}

pub fn __new__(class : Gc<Class_ty>, env: Gc<Env>) -> Gc<_Object> {
    Gc::new(_Object::Its(Instance_ty{
        // env    : env.clone(),
        parent : class.clone(),
        locals : Vec::new(),
    }))
}

// Environment
#[derive(Trace)]
pub struct Env {
    pub classes : Vec<Gc<Class_ty>>,
    pub global_objs: Vec<Gc<_Object>>,
}

impl Env {
    pub fn __new__(&self, x : usize) -> Gc<_Object> {
        Gc::new(_Object::Its(Instance_ty{
            // env    : env.clone(),
            parent : self.classes[x].clone(),
            locals : Vec::new(),
        }))
    }
}

    
pub fn __class_decl__(code: &Vec<SCode>, n_attrs: usize, n_methods: usize, start: usize) -> (Gc<Class_ty>, usize) {
    let mut pc : usize = start;
    let mut attrs = Vec::new();
    let mut methods = Vec::new();

    for _ in 0..n_attrs {
        match code[pc] {
            PUSH_STR(ref s) => {
                attrs.push(s.clone());
                pc = pc + 1;
            },
            _ => break
        }
    }

    pc = pc + 1;


    let mut cb = Vec::new();
    let mut flag = false;

    for _ in 0..n_methods {

        while true {
            match code[pc] {
                FRMEND => break,
                _ => {
                    cb.push(code[pc].clone());
                    pc = pc + 1;
                }
            }
        }

        pc = pc + 1;

        match code[pc] {
            PUSH_STR(ref s) => {
                methods.push(Method_ty {
                    _name  : s.clone(),
                    _code  : cb.clone(),
                });
            },
            _ => { assert!(false); }
        }

        cb = Vec::new();

        pc = pc + 1;
    }

    (Gc::new(Class_ty {
                // name : Gc::new(name.to_string()),
                methods : methods,
                attrs   : attrs,
            }) , pc)
}



pub trait Object {
    fn call(&self, &str, Vec<Gc<_Object>>, &Gc<Env>, &mut Vec<Gc<_Object>>) -> Gc<_Object>;
    fn tyof(&self) -> &str;
}

