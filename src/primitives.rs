/* 
 * Instance objects
 *
 */

use super::*;
use gc::*;
use object::*;
use env::Env;
use dispatch::DynObj;

#[derive(Trace)]
pub struct IntObj {
    val: Integer,
}

#[derive(Trace)]
pub struct BoolObj {
    val: bool,
}

#[derive(Trace)]
pub struct StrObj {
    val: String
}


impl IntObj {
    fn new(i: Integer) -> Gc<DynObj> {
        Gc::new(DynObj::Int(IntObj { val : i }))
    }
}

impl BoolObj {
    fn new(b: bool) -> Gc<DynObj> {
        Gc::new(DynObj::Bool(BoolObj { val : b }))
    }
}


impl StrObj {
    fn new(s: String) -> Gc<DynObj> {
        Gc::new(DynObj::Str(StrObj { val : s }))
    }
}


impl Object for IntObj {
    fn invoke(&mut self, name : &str, args: Vec<Gc<DynObj>>, _ : &Env) -> Option<Gc<DynObj>> {
        match name {
            "add" => {
                let ref b = *args[1];
                match b {
                    &DynObj::Int(ref intobj) => Some(IntObj::new(self.val + intobj.val)),
                    other => {
                        println!("invalid type for add: {:?}", other.tyof());
                        return None;
                    }
                } 
            },
            "print" => {
                print!("{}", self.val);
                return None;
            },
            other => invoke_fail("IntObj", other)
        }
    }

    fn tyof(&self) -> String { format!("<int>({})", self.val) }
}


impl Object for BoolObj {
    fn invoke(&mut self, name : &str, _: Vec<Gc<DynObj>>, _ : &Env) -> Option<Gc<DynObj>> {
        match name {
            "not" => Some(BoolObj::new(!self.val)),
            "print" => {
                print!("{}", self.val);
                return None;
            },
            other => invoke_fail("BoolObj", other)
        }
    }

    fn tyof(&self) -> String { format!("<bool>({})", self.val) }
}

impl Object for StrObj {
    fn invoke(&mut self, name : &str, args: Vec<Gc<DynObj>>, _ : &Env) -> Option<Gc<DynObj>> {
        match name {
            "print" => {
                print!("{}", self.val);
                return None;
            },
            other => invoke_fail("StrObj", other)
        }
    }

    fn tyof(&self) -> String { format!("<str>({})", self.val) }
}
