
use gc::*;
use super::*;
use super::object::*;
use super::object::_Object::*;
use super::classty::*;

///////////////// Int //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Int_ty {
    _i : int,
}

impl Int_ty {
    pub fn new(i: int) -> Gc<_Object> {
        Gc::new(Int(Int_ty {
            _i : i
        }))
    }

    pub fn unbox(&self) -> int {
        self._i
    }
}


impl Object for Int_ty {
    fn call(&self, name : &str, args: Vec<Gc<_Object>>, env : &Gc<Env>, globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            "add" => {
                let ref b = *args[0];
                match b {
                    &Int(ref intty) => Int_ty::new(self._i + intty._i),
                    o => {
                        println!("invalid type for add: {:?}", o.tyof());
                        Gc::new(Non)
                    }
                } 
            },
            "__print__" => {
                print!("{}", self._i);
                Gc::new(Non)
            },
            m => {
                println!("no such method {:?}", m);
                Gc::new(Non)
            }
        }
    }

    fn tyof(&self) -> &str { "<int>" }
}


///////////////// Array //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Array_ty {
    vector : GcCell<Vec<Gc<_Object>>>,
}

impl Array_ty {
    pub fn new() -> Gc<_Object> {
        Gc::new(Arr(Array_ty {
            vector : GcCell::new(Vec::with_capacity(100))
        }))
    }
}

impl Object for Array_ty {
    fn call(&self, name: &str, args: Vec<Gc<_Object>>, env : &Gc<Env>, globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            "at" =>  {
                let ref n = *args[0];
                match n {
                    &Int(ref intty) => { 
                        let i = intty._i as usize;
                        let elem = self.vector.borrow()[i].clone();
                        elem
                    }
                    o => {
                        println!("invalid type for indexing: {:?}", o.tyof());
                        Gc::new(Non)
                    }
                }
            },
            "push" => {
                for arg in args {
                    self.vector.borrow_mut().push(arg.clone());
                }

                Gc::new(Non)
            },
            m => {
                println!("no such method {:?}", m);
                Gc::new(Non)
            }
        }
    }

    fn tyof(&self) -> &str { "<array>" }
}


///////////////// String //////////////////
#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct String_ty {
    string : Gc<String>,
}

impl String_ty {
    pub fn new(s: Gc<String>) -> Gc<_Object> {
        Gc::new(_Object::Str(String_ty{
            string : s
        }))
    }
}

impl Object for String_ty {
    fn call(&self, name: &str, args: Vec<Gc<_Object>>, env : &Gc<Env>, globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        println!("no such method {:?}", name);
        Gc::new(Non)
    }

    fn tyof(&self) -> &str { "<string>" }
}
