use ast::*;
use std::collections::HashMap;
use gc::*;
use std::ops::Add;
use self::PrimitiveType::*;

#[derive(Debug, Copy, Clone, Trace)]
pub enum PrimitiveType {
    PrimInt(i32),
}

impl Add<PrimitiveType> for PrimitiveType {
    type Output = PrimitiveType;
    fn add(self, other: PrimitiveType) -> PrimitiveType {
        match self {
            PrimInt(i) => {
                match other {
                    PrimInt(j) => PrimInt(i + j)
                }
            }
        }
    }
}


fn call_builtin(prim : &PrimitiveType, name : &str, params : &HashMap<&str, & mut Object>) -> Option<Object>{
    unimplemented!()
}

#[derive(Trace)]
pub struct Function {
    pub arguments : Vec<String>,
    pub body : Vec<Statement>,
}

pub struct Template {
    pub name : String,
    pub methods : HashMap<String, Function>,
}

#[derive(Trace)]
pub struct Complex {
    #[unsafe_ignore_trace]
    pub template : Box<Template>,
    pub attributes : Vec<Box<Object>>,
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
    pub objects : &'a mut HashMap<String, Gc<Object>>,
}
