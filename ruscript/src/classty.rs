use gc::*;
use framety::*;
use super::*;
use super::object::*;
use super::object::_Object::*;
use super::stackcode::*;
use super::stackcode::SCode::*;

///////////////// Class //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Method_ty {
    pub name : String,
    pub frame : Gc<Frame_ty>,
}

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Class_ty {
    name    : String,
    methods : Vec<Method_ty>,
    attrs   : Vec<String>,
}


impl Class_ty {
    pub fn new(name : &str, ms : Vec<Method_ty>, attrs : Vec<String>, env: &mut Env) {
        env.classes.push(Gc::new(Class_ty {
            name : name.to_string(),
            methods : ms,
            attrs : attrs,
        }));
    }
}

impl Object for Class_ty {
    #[allow(unused_variables)]
    fn call(&self, name: &str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
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
    fn call(&self, name: &str, args: Vec<Gc<_Object>>) -> Gc<_Object> {        
        for m in self.parent.methods.iter() {
            if m.name == name.to_string() {
                return m.frame.call("__run__", args.clone());
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
    pub globals : Vec<Gc<_Object>>,
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

fn __class_decl__(env: &mut Env, code: &Vec<SCode>, n_attrs: usize, n_methods: usize, start: usize) -> usize {
    let mut pc : usize = start;
    let mut attrs = Vec::new();

    while true {
        match code[pc] {
            PUSH_STR(ref s) => {
                attrs.push(s.clone());
                pc = pc + 1;
            },
            _ => break
        }
    }

    let mut cb = Vec::new();
    let mut flag = false;

    while true {
        match code[pc] {
            FRMEND => break,
            _ => {
                cb.push(code[pc].clone());
            }
        }
    }

    pc
}
