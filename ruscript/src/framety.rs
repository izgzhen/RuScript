use stackcode::*;
use stackcode::SCode::*;
use gc::*;
use super::*;
use super::object::*;
use super::object::_Object::*;
use super::classty::*;
use super::primty::*;
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
            env : env.clone(),
            stack : GcCell::new(Vec::new())
        })
    }
}

impl Object for Frame_ty {
    fn call(&self, name: &str, args: Vec<Gc<_Object>>, env : &Gc<Env>) -> Gc<_Object> {
        match name {
            "__run__" => {
                for i in 0..self.codeblock.len() {
                    let ref inst = self.codeblock[i];
                    interprete(&inst, &args, &self.stack, env);
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
