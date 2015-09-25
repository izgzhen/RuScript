use gc::*;

use self::_Object::*;
use self::OpCode::*;

// used for profiling
use std::cell::Cell;
thread_local!(static COUNT_DROPPED: Cell<u8> = Cell::new(0u8));

pub type ObjIdentTy = i32;
pub type ArgIdentTy = i8;

#[allow(non_camel_case_types)]
pub type int = i64;


#[derive(Trace)]
pub enum OpCode {
    PUSH(ObjIdentTy), // Use number as identifier, needs some translation to generate efficent opcode as well as symbal table
    ADD, // "Add" two objects together
    CALL(ObjIdentTy, String, ArgIdentTy), // Receiver, Method name, and number of arguments
    RET // return the stack top
} 

pub trait Object {
    fn call(&self, &str, Vec<Gc<_Object>>) -> Gc<_Object>;
    fn tyof(&self) -> &str;
}

#[derive(Trace)]
pub enum _Object {
    Int(Int_ty),
    Arr(Array_ty),
    Frm(Frame_ty),
    Its(Instance_ty),
    Cls(Class_ty),
    Non,
}

///////////////// Dispatch //////////////////

impl Object for _Object {
    fn call(&self, name : &str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
        match self {
            &Int(ref intty) => intty.call(name, args),
            &Arr(ref arrty) => arrty.call(name, args),
            &Frm(ref frmty) => frmty.call(name, args),
            &Cls(ref clsty) => clsty.call(name, args),
            &Its(ref itsty) => itsty.call(name, args),
            &Non => {
                println!("None object is not cllable");
                Gc::new(Non)
            },
        }
    }

    fn tyof(&self) -> &str {
        match self {
            &Int(ref intty) => intty.tyof(),
            &Arr(ref arrty) => arrty.tyof(),
            &Frm(ref frmty) => frmty.tyof(),
            &Cls(ref clsty) => clsty.tyof(),
            &Its(ref itsty) => itsty.tyof(),
            &Non => { "<None>" },
        }
    }
}

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
    fn call(&self, name : &str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
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
    fn call(&self, name: &str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
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

///////////////// Frame //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Frame_ty {
    codeblock : Box<Vec<OpCode>>,
    globals   : Box<Vec<_Object>>,
    stack     : GcCell<Vec<Gc<_Object>>>,
}

impl Frame_ty {
    pub fn new(cb : Box<Vec<OpCode>>, globals: Box<Vec<_Object>>) -> Gc<_Object> {
        Gc::new(_Object::Frm(Frame_ty{
            codeblock : cb,
            globals : globals,
            stack : GcCell::new(Vec::new())
        }))
    }
}

impl Object for Frame_ty {
    fn call(&self, name: &str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            "__run__" => {
                for i in 0..self.codeblock.len() {
                    // let ref inst = self.codeblock.borrow()[i];
                    let ref inst = self.codeblock[i];
                    match inst {
                        &PUSH(x) => {
                            self.stack.borrow_mut().push(args[x as usize].clone());
                        },
                        &ADD => {
                            let a = self.stack.borrow_mut().pop().unwrap();
                            let b = self.stack.borrow_mut().pop().unwrap();
                            let sum = a.call("add", vec![b]);
                            self.stack.borrow_mut().push(sum);
                        },
                        &CALL(recv, ref method, narg) => {
                            let ref obj = self.globals[recv as usize];

                            let mut params = Vec::new();

                            for _ in 0..narg {
                                let x = self.stack.borrow_mut().pop().unwrap();
                                params.push(x.clone());
                            }
                            return obj.call(method, params);
                        },
                        &RET => {
                            let x = self.stack.borrow_mut().pop().unwrap();
                            return x;
                        }
                        // _ => {
                        //     println!("unknown instruction");
                        // }
                    }
                }

                Gc::new(Non)
            },
            m => {
                println!("no such method {:?}", m);
                Gc::new(Non)
            }
        }
    }

    fn tyof(&self) -> &str { "<frame>" }
}


///////////////// Class //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace)]
struct Method_ty {
    name : String,
    frame : Gc<Frame_ty>,
}

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Class_ty {
    name    : String,
    methods : Vec<Method_ty>,
    attrs   : Vec<String>,
}


impl Class_ty {
    fn new(name : &str, ms : Vec<Method_ty>, attrs : Vec<String>) -> Gc<_Object> {
        Gc::new(_Object::Cls(Class_ty {
            name : name.to_string(),
            methods : ms,
            attrs : attrs,
        }))
    }
}

impl Object for Class_ty {
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
                m.frame.call("__run__", args.clone());
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
fn __new__(class : &Gc<Class_ty>) -> Gc<_Object> {
    Gc::new(_Object::Its(Instance_ty{
        parent : class.clone(),
        locals : Vec::new(),
    }))
}


// // // // // // // //  MISC // // // // // // // // // 

impl Drop for _Object {
    fn drop(&mut self) {
        COUNT_DROPPED.with(|count| count.set(count.get() + 1)); 
        println!("Dropping Object");
    }
}


