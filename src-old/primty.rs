
use gc::*;
use super::*;
use super::object::*;
use super::object::_Object::*;
use super::classty::*;

///////////////// Int //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace, Debug)]
pub struct Int_ty {
    _i : Integer,
}

impl Int_ty {
    pub fn new(i: Integer) -> Gc<_Object> {
        Gc::new(Int(Int_ty {
            _i : i
        }))
    }

    pub fn unbox(&self) -> Integer {
        self._i
    }
}


impl Object for Int_ty {
    fn call(&self, name : &str, args: Vec<Gc<_Object>>, _ : &Gc<Env>, _: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            "add" => {
                let ref b = *args[1];
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
            m => call_fail("Int_ty", m)
        }
    }

    fn tyof(&self) -> String { format!("<int>({})", self._i) }
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
    fn call(&self, name: &str, args: Vec<Gc<_Object>>, _ : &Gc<Env>, _: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            "at" =>  {
                let ref n = *args[1];
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
                for i in 1..args.len() {
                    self.vector.borrow_mut().push(args[i].clone());
                }

                Gc::new(Non)
            },
            m => call_fail("Array_ty", m)
        }
    }

    fn tyof(&self) -> String { "<array>".to_string() }
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
    fn call(&self, name: &str, _: Vec<Gc<_Object>>, _ : &Gc<Env>, _: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            "__print__" => { // Just literal printing
                print!("{}", *(self.string));
                Gc::new(Non)
            },
            m => call_fail("String_ty", m)
        }
    }

    fn tyof(&self) -> String { format!("<string>({})", *self.string) }
}
