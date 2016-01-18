
use stackcode::*;
use gc::*;
use super::object::*;
use super::object::_Object::*;
use super::classty::*;
use super::interprete::*;

///////////////// Frame //////////////////

#[allow(non_camel_case_types)]
#[derive(Trace)]
pub struct Frame_ty {
    codeblock : Box<Vec<SCode>>,
    env       : Gc<Env>,
    stack     : GcCell<Vec<Gc<_Object>>>,
}

impl Frame_ty {
    pub fn new(cb : Box<Vec<SCode>>, env : &Gc<Env>) -> Gc<Frame_ty> {
        Gc::new(Frame_ty{
            codeblock : cb,
            env   : env.clone(),
            stack : GcCell::new(Vec::new()),
        })
    }
}

impl Object for Frame_ty {
    fn call(&self, name: &str, args: Vec<Gc<_Object>>, env: &Gc<Env>, globals: &mut Vec<Gc<_Object>>) -> Gc<_Object> {
        match name {
            "__run__" => {
                let mut locals = vec![];
                for o in &args {
                    locals.push(o.clone());
                }
                let void = super::primty::Int_ty::new(0);
                for _ in args.len()..super::GLOBAL_MAXSIZE {
                    locals.push(void.clone());
                }

                let mut i : usize = 0;
                while i < self.codeblock.len() {
                    let ref inst = self.codeblock[i];

                    println!("interpreting: {:?}", inst);
                    match inst {
                        &SCode::CALLL(recv, ref method, narg) => {
                            let ref obj = locals[recv as usize].clone();
                            let mut params = Vec::new();
                            params.push(locals[recv as usize].clone()); // this pointer
                            for _ in 0..narg {
                                let x = self.stack.borrow_mut().pop().unwrap();
                                params.push(x.clone());
                            }
                            return obj.call(method, params, env, globals);
                        },
                        &SCode::CALLG(recv, ref method, narg) => {
                            let ref obj = globals[recv as usize].clone();

                            let mut params = Vec::new();
                            params.push(globals[recv as usize].clone()); // this pointer

                            for _ in 0..narg {
                                let x = self.stack.borrow_mut().pop().unwrap();
                                params.push(x.clone());
                            }
                            return obj.call(method, params, env, globals);
                        },
                        &SCode::RET => {
                            return self.stack.borrow_mut().pop().unwrap();
                        },
                        &SCode::PUSHSELF => {
                            self.stack.borrow_mut().push(args[0].clone());
                        },
                        inst => {
                            match interprete(i, &inst, &mut locals, &self.stack, env, globals) {
                                Some(new_i) => i = new_i,
                                None => i = i + 1,
                            }
                        }
                    }

                    // dump_stack(&self.stack)
                }

                Gc::new(Non)
            },
            m => {
                println!("no such method {:?}", m);
                Gc::new(Non)
            }
        }
    }

    fn tyof(&self) -> String { "<frame>".to_string() }
}
