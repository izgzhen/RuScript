use ast::*;
use std::collections::HashMap;
use gc::*;
use std::ops::Add;
use self::PrimitiveType::*;


// used for profiling
use std::cell::Cell;
thread_local!(static COUNTER: Cell<u8> = Cell::new(0u8));




trait Object {
    call(name, args, dest)
    new
    typeof
}

impl Object for Int_ty { // Primitive
    call(self, name, args, dest) {
        // sometemplat requires format of args
        match name {
            "add" => dest = box (self + obj_to_int!(args[0])); // Just add, release the old and apply for a new
            "..." => error "no such methods" // Can be done in macro as well?
        }
    }

    new(int i) {
        box i
    }

    typeof() {
        "<int>"
    }
}

impl Object for Array_ty { // hetero arrays, just an array of typeless boxes
    call(self, name, args, dest) {
        match name {
            "index" => dest = Array::vector[i];
            "..." => error "no such methods"
        }
    }

    new() {
        box empty_array
    }

    typeof() { "<array> "}
}



struct Array {
    vector :: Vec<Gc<_Object>>
}

enum _Object {
    Int(Int_ty, i32),
    Arr(Array_ty, Box<Array>),
    // ... and more
    Frm(Frame_ty, Box<Frame>)
}


impl Object for Frame_ty {
    call(self /* */)  // Some reflection etc.

    new(Frame) //



}


struct Frame {
    box code_obj, // stack code
    box last_frame,
    box locals // Map<string, PyObj>,
    box globals
    box stack
    box ret
}




impl Frame {
    execute {
        loop {
            next_code = code_obj.next()
            if next_code == None: break;
            else  match next_code {
                PUSH(x) -> stack.push(locals[x]),
                ADD(x, y) -> {
                    let a = locals[x]
                    let b = locals[y]
                    let c = None
                    a.call("add", [b], c)
                    stack.push(c)
                },
                Bind(name) -> {
                    locals.push_back(name, stack.top())
                },
                Call(obj_name, method_name, i) -> {
                    match globals.lookup(obj_name) {
                        Some(obj) -> {
                            let args = []
                            while(i--){
                                args.push_back(i)
                            }
                            let mut ret;
                            obj.call(method_name. args, ret);
                            push_back(ret);
                        }
                    }
                }
                // ...
            }
        }
    }

}

struct Class {
    name,
    Map<string, opcode> // methods
    Map<string, _Object> // attributes
}

impl Class {
    fn instantiate() -> GenericObj
}

struct GenericObj {
    box class, // parent
    self = Map<string, _Object> // local storage
}


impl GenericObj for Object {
    fn call (self, name, args, dest) {
        let frame = Frame::new(self.class.methods[name]);
        frame.locals.add(args);
        frame.locals.add(self.attributes)
        frame.execute()
        dest = box frame.ret.copy()
    }

    fn new(class) {
        class.instantiate()
    }

    fn typeof() { "<class:" + self.class.name + ">"}
}


fn interprete(opcode){
    let frameObj = Frame::new(opcode);
    frameObj.execute()

}

enum OpCode {
    PUSH()
    Call()
    Bind()
    ADD()
}


fn interpreteByte(byte) -> Vec<OpCode> {
    // ... some serailization ...
}




impl Drop for Object {
    fn drop(&mut self) {
        println!("Dropping Object");
    }
}



