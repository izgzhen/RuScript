//!
//! Primitive types object
//!

use super::*;
use gc::*;
use object::*;
use env::Env;
use dispatch::DynObj;
use self::ListObj::*;

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

#[derive(Trace, Clone)]
pub enum ListObj {
    Empty,
    Cons {
        hd: Gc<DynObj>,
        tl: Gc<DynObj>,
    },
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

impl ListObj {
    pub fn new(head: Gc<DynObj>, tail: Gc<DynObj>) -> Gc<DynObj> {
        Gc::new(DynObj::List(Cons {
            hd : head,
            tl : tail,
        }))
    }

    #[allow(unused_variables)]
    pub fn len(&self) -> Integer {
        match self {
            &Empty => 0,
            &Cons{ ref hd, ref tl } => {
                match **tl {
                    DynObj::List(ref l) => 1 + l.len(),
                    _ => panic!("illegal type in list"),
                }
            }
        }
    }

    pub fn at(&self, i: Integer) -> Gc<DynObj> {
        match self {
            &Empty => panic!("Can't access element of an empty list"),
            &Cons{ ref hd, ref tl } => {
                if i == 0 {
                    hd.clone()
                } else {
                    match **tl {
                        DynObj::List(ref l) => l.at(i),
                        _ => panic!("illegal type in list"),
                    }
                }
            }
        }
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
            "le" => {
                let b = stack.pop().unwrap();
                match *b {
                    DynObj::Int(ref intobj) => {
                        stack.push(BoolObj::new(self.val <= intobj.val));
                    },
                    ref other => {
                        panic!("invalid type for le: {:?}", other.tyof());
                    }
                }
            }
            other => invoke_fail("IntObj", other)
        }
    }

    fn to_string(&self) -> String { self.val.to_string() }


    fn tyof(&self) -> String { format!("<int>({})", self.val) }
}


impl Object for BoolObj {
    /// Built-in functions: `not`, `print`
    fn invoke(&self, name : &str, stack: &mut Vec<Gc<DynObj>>, _ : &Env){
        match name {
            "not" => {
                stack.push(BoolObj::new(!self.val));
            },
            other => invoke_fail("BoolObj", other)
        }
    }

    fn to_string(&self) -> String {
        self.val.to_string()
    }

    fn tyof(&self) -> String { format!("<bool>({})", self.val) }
}

impl Object for StrObj {
    /// Built-in functions: `print`
    fn invoke(&self, name : &str, _: &mut Vec<Gc<DynObj>>, _ : &Env) {
        match name {
            other => invoke_fail("StrObj", other)
        }
    }

    fn to_string(&self) -> String {
        self.val.to_string()
    }

    fn tyof(&self) -> String { format!("<str>({})", self.val) }
}


impl Object for ListObj {
    /// Built-in functions: `print`
    fn invoke(&self, name : &str, stack: &mut Vec<Gc<DynObj>>, _ : &Env) {
        match name {
            "cons" => {
                let b = stack.pop().unwrap();
                stack.push(ListObj::new(b, Gc::new(DynObj::List(self.clone()))));
            },
            "len" => {
                stack.push(IntObj::new(self.len()));
            },
            "at" => {
                let b = stack.pop().unwrap();
                match *b {
                    DynObj::Int(ref intobj) => {
                        stack.push(self.at(intobj.val));
                    },
                    ref other => {
                        panic!("invalid type for at: {:?}", other.tyof());
                    }
                }
            },
            other => invoke_fail("ListObj", other)
        }
    }

    fn to_string(&self) -> String {
        match self {
            &Empty => "[]".to_string(),
            &Cons{ ref hd, ref tl } => {
                let mut s = String::from("[");
                s.push_str(&hd.to_string());

                match **tl {
                    DynObj::List(ref l) => {
                        match *l {
                            Empty => {
                                s.push(']');
                                s
                            },
                            _ => {
                                s.push_str(", ");
                                s.push_str(&tl.to_string());
                                s
                            }
                        }
                    },
                    _ => panic!("illegal type in list"),
                }
            }
        }
    }

    fn tyof(&self) -> String { "<list>".to_string() }
}
