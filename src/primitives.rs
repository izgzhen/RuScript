//! 
//! Primitive types object
//!

use super::*;
use gc::*;
use object::*;
use env::Env;
use dispatch::DynObj;

#[derive(Trace, Clone)]
pub struct IntObj {
    pub val: Integer,
}

#[derive(Trace, Clone)]
pub struct BoolObj {
    pub val: bool,
}

#[derive(Trace, Clone)]
pub struct StrObj {
    pub val: String
}


impl IntObj {
    pub fn new(i: Integer) -> Gc<DynObj> {
        Gc::new(DynObj::Int(IntObj { val : i }))
    }
}

impl BoolObj {
    pub fn new(b: bool) -> Gc<DynObj> {
        Gc::new(DynObj::Bool(BoolObj { val : b }))
    }
}


impl StrObj {
    pub fn new(s: String) -> Gc<DynObj> {
        Gc::new(DynObj::Str(StrObj { val : s }))
    }
}


impl Object for IntObj {
    /// Built-in functions: `add`, `print`
    fn invoke(&self, name : &str, stack: &mut Vec<Gc<DynObj>>, _ : &Env) {
        match name {
            "add" => {
                let b = stack.pop().unwrap();
                match *b {
                    DynObj::Int(ref intobj) => {
                        stack.push(IntObj::new(self.val + intobj.val));
                    },
                    ref other => {
                        panic!("invalid type for add: {:?}", other.tyof());
                    }
                } 
            },
            "print" => {
                print!("{}", self.val);
            },
            other => invoke_fail("IntObj", other)
        }
    }

    fn tyof(&self) -> String { format!("<int>({})", self.val) }
}


impl Object for BoolObj {
    /// Built-in functions: `not`, `print`
    fn invoke(&self, name : &str, stack: &mut Vec<Gc<DynObj>>, _ : &Env){
        match name {
            "not" => {
                stack.push(BoolObj::new(!self.val));
            },
            "print" => {
                print!("{}", self.val);
            },
            other => invoke_fail("BoolObj", other)
        }
    }

    fn tyof(&self) -> String { format!("<bool>({})", self.val) }
}

impl Object for StrObj {
    /// Built-in functions: `print`
    fn invoke(&self, name : &str, _: &mut Vec<Gc<DynObj>>, _ : &Env) {
        match name {
            "print" => {
                print!("{}", self.val);
            },
            other => invoke_fail("StrObj", other)
        }
    }

    fn tyof(&self) -> String { format!("<str>({})", self.val) }
}
