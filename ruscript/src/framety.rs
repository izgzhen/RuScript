use stackcode::*;
use stackcode::SCode::*;
use gc::*;
use super::*;
use super::object::*;
use super::object::_Object::*;


///////////////// Frame //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Frame_ty {
    codeblock : Box<Vec<SCode>>,
    globals   : Box<Vec<Gc<_Object>>>,
    stack     : GcCell<Vec<Gc<_Object>>>,
}

impl Frame_ty {
    pub fn new(cb : Box<Vec<SCode>>, globals: Box<Vec<Gc<_Object>>>) -> Gc<Frame_ty> {
        Gc::new(Frame_ty{
            codeblock : cb,
            globals : globals,
            stack : GcCell::new(Vec::new())
        })
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
