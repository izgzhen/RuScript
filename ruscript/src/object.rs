use std::collections::HashMap;

#[macro_use]
use gc::*;

use self::_Object::*;

// used for profiling
use std::cell::Cell;
thread_local!(static COUNTER: Cell<u8> = Cell::new(0u8));



type ObjIdentTy = i32;
type ArgIdentTy = i8;
type int = i64;

enum OpCode {
    PUSH(ObjIdentTy), // Use number as identifier, needs some translation to generate efficent opcode as well as symbal table
    ADD(ObjIdentTy, ObjIdentTy), // "Add" two objects together
    CALL(ObjIdentTy, String, ArgIdentTy), // Receiver, Method name, and number of arguments
    BIND(ObjIdentTy), // Create a new local variable for fast access
} 

trait Object {
    fn call(&self, &'static str, Vec<Gc<_Object>>) -> Gc<_Object>;
    fn tyof(&self) -> &'static str;
}

#[derive(Clone, Trace)]
enum _Object {
    Int(Int_ty),
    // Arr(Box<Array_ty>, Gc<Array>),
    // Frm(Box<Frame_ty>, Gc<Frame>),
    // Gen(Gc<GenericObj>),
    // Cls(Gc<ClassObj>),
    Non,
}


///////////////// Int //////////////////

#[derive(Clone, Trace)]
struct Int_ty {
    _i : int,
}

impl Int_ty {
    fn new(i: int) -> Gc<_Object> {
        Gc::new(Int(Int_ty {
            _i : i
        }))
    }
}


impl Object for Int_ty {
    fn call(&self, name : &'static str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
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

    fn tyof(&self) -> &'static str { "<int>" }
}

///////////////// Dispatching //////////////////
impl Object for _Object {
    fn call(&self, name : &'static str, args: Vec<Gc<_Object>>) -> Gc<_Object> {
        match self {
            &Int(ref intty) => intty.call(name, args),
            &Non => {
                println!("None object is not cllable");
                Gc::new(Non)
            }
        }
    }

    fn tyof(&self) -> &'static str {
        match self {
            &Int(ref intty) => intty.tyof(),
            &Non => { "<None>" }
        }
    }
}


///////////////// Array //////////////////

// impl Object for Array_ty { // hetero arrays, just an array of typeless boxes
//     call(self, name, args, dest) {
//         match name {
//             "index" => dest = Array::vector[i];
//             "..." => error "no such methods"
//         }
//     }

//     tyof() { "<array> "}
// }



// struct Array {
//     vector :: Vec<Gc<_Object>>
// }


// impl Object for Frame_ty {
//     call(self /* */)  // Some reflection etc.

//     new(Frame) //



// }


// struct Frame {
//     box code_obj, // stack code
//     box last_frame,
//     box locals // Map<string, PyObj>,
//     box globals
//     box stack
//     box ret
// }




// impl Frame {
//     execute {
//         loop {
//             next_code = code_obj.next()
//             if next_code == None: break;
//             else  match next_code {
//                 PUSH(x) -> stack.push(locals[x]),
//                 ADD(x, y) -> {
//                     let a = locals[x]
//                     let b = locals[y]
//                     let c = None
//                     a.call("add", [b], c)
//                     stack.push(c)
//                 },
//                 BIND(name) -> {
//                     locals.push_back(name, stack.top())
//                 },
//                 CALL(obj_name, method_name, i) -> {
//                     match globals.lookup(obj_name) {
//                         Some(obj) -> {
//                             let args = []
//                             while(i--){
//                                 args.push_back(i)
//                             }
//                             let mut ret;
//                             obj.call(method_name. args, ret);
//                             push_back(ret);
//                         }
//                     }
//                 }
//                 // ...
//             }
//         }
//     }

// }

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
        println!("Dropping Object");
    }
}



