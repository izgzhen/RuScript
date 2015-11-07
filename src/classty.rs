use gc::*;
use framety::*;
use super::object::*;
use super::object::_Object::*;
use super::stackcode::*;
use super::stackcode::SCode::*;
use std::fmt::Debug;

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
    methods : Vec<Method_ty>,
    attrs   : Vec<Gc<String>>,
}

impl Object for Class_ty {
    #[allow(unused_variables)]
    fn call(&self, name: &str, args: Vec<Gc<_Object>>, env: &Gc<Env>, globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            m => call_fail("Class_ty", m)
        }
    }

    fn tyof(&self) -> String { "<class>".to_string() }
}


///////////////// Instance //////////////////


#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Instance_ty {
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

        call_fail("Instance_ty", name)
    }

    fn tyof(&self) -> String {
        "<instance>".to_string()
    }
}

pub fn __new__(class : Gc<Class_ty>, _: Gc<Env>) -> Gc<_Object> {
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

    for _ in 0..n_methods {

        loop {
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
    fn access(&self, name: &str) -> Gc<_Object> {
        access_fail(&self.tyof(), name)
    }

    fn access_i(&self, index: usize) -> Gc<_Object> {
        access_fail(&self.tyof(), index)
    }

    fn tyof(&self) -> String;
}

pub fn call_fail(ty: &str, name: &str) -> Gc<_Object> {
    println!("{:?} no such method {:?}", ty, name);
    Gc::new(Non)
}

pub fn access_fail<N>(ty: &str, name: N) -> Gc<_Object> where N: Debug {
    println!("{:?} no such attr {:?}", ty, name);
    Gc::new(Non)
}

pub fn dump_stack(stack: &GcCell<Vec<Gc<_Object>>>) {
    print!("stack size: {:?}, content: ", stack.borrow().len());
    for x in stack.borrow().iter() {
        print!("{} ", x.tyof());
    }
    println!("");
}

