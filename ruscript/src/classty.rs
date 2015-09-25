use gc::*;
use framety::*;
use super::*;
use super::object::*;
use super::object::_Object::*;

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
    pub fn new(name : &str, ms : Vec<Method_ty>, attrs : Vec<String>) -> Gc<Class_ty> {
        Gc::new(Class_ty {
            name : name.to_string(),
            methods : ms,
            attrs : attrs,
    })
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

// Used globally = = ||||||
pub fn __new__(class : &Gc<Class_ty>) -> Gc<_Object> {
    Gc::new(_Object::Its(Instance_ty{
        parent : class.clone(),
        locals : Vec::new(),
    }))
}

