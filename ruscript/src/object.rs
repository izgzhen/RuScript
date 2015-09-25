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
    // Gen(Gc<GenericObj>),
    // Cls(Gc<ClassObj>),
    Non,
}

///////////////// Dispatch //////////////////

impl Object for _Object {
    fn call(&self, name : &str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
        match self {
            &Int(ref intty) => intty.call(name, args),
            &Arr(ref arrty) => arrty.call(name, args),
            &Frm(ref frmty) => frmty.call(name, args),
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



// struct Class {
//     name,
//     Map<string, opcode> // methods
//     Map<string, _Object> // attributes
// }

// impl Class {
//     fn instantiate() -> GenericObj
// }

// impl Object for Class {
//     fn call(self, name, args, dest) {
//         let frame = Frame::new(self.methods[name]);
//         frame.locals.add(args);
//         frame.locals.add(self.attributes);
//         frame.execute();
//         dest = box frame.ret.copy();
//     }

//     fn tyof() { "<class>" }
// }

// struct GenericObj {
//     box class, // parent
//     self = Map<string, _Object> // local storage
// }


// impl GenericObj for Object {
//     fn call (self, name, args, dest) {
//         let frame = Frame::new(self.class.methods[name]);
//         frame.locals.add(args);
//         frame.locals.add(self.attributes);
//         frame.execute();
//         dest = box frame.ret.copy();
//     }

//     fn tyof() { "<class:" + self.class.name + ">"}
// }


// fn interprete(opcode){
//     let frameObj = Frame::new(opcode);
//     frameObj.execute()
// }



// fn interpreteByte() -> Vec<OpCode> {
//     // ... some serailization ...
// }


impl Drop for _Object {
    fn drop(&mut self) {
        COUNT_DROPPED.with(|count| count.set(count.get() + 1)); 
        println!("Dropping Object");
    }
}



