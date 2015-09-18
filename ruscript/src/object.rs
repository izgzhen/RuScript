use ast::*;
use std::collections::HashMap;
use gc::*;

#[derive(Debug, Copy, Clone, Trace)]
pub enum PrimitiveType {
    PrimInt(i32),
}

fn call_builtin(prim : &PrimitiveType, name : &str, params : &HashMap<&str, & mut Object>) -> Option<Object>{
    unimplemented!()
}

#[derive(Trace)]
pub struct Function {
    arguments : Vec<String>,
    body : Vec<Statement>,
}

pub struct Template {
    name : String,
    methods : HashMap<String, Function>,
}

#[derive(Trace)]
pub struct Complex {
    #[unsafe_ignore_trace]
    template : Box<Template>,
    attributes : Vec<Box<Complex>>,
}

#[derive(Trace)]
pub enum Object {
    PrimObj(PrimitiveType),
    FuncObj(Function),
    CompObj(Complex),
}

trait Entity {
    fn call<'a>(&'a self, Option<&str>, &HashMap<&str, &'a mut Object>) -> Option<Object>;
}

impl Entity for PrimitiveType {
    fn call<'a>(&'a self, target : Option<&str>, params : &HashMap<&str, &'a mut Object>) -> Option<Object> {
        match target {
            Some(name) => {
                call_builtin(&self, name, params)
            },
            None => {
                match self {
                    &PrimitiveType::PrimInt(i) => Some(Object::PrimObj(PrimitiveType::PrimInt(i))),
                }
            },
        }
    }
}



pub struct Environment<'a> {
    pub primitives : &'a mut HashMap<String, Gc<Object>>,
}
