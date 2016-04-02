//!
//! Instance object
//!

use gc::*;
use object::*;
use class::Class;
use env::Env;
use dispatch::*;

#[derive(Trace, Clone)]
pub struct InstanceObj {
    /// Index of parent class, from which the variable is initialized
    pub cls   : usize,
    /// Attributes values, in the same order as declared
    pub attrs : Vec<Cgc<DynObj>>,
}

impl InstanceObj {
    /// Instance initializer. The attributes are parameter of initialization and put in the
    /// same way as common functions
    pub fn new(attrs_len: usize, cls_idx: usize, stack: &mut Vec<Cgc<DynObj>>) -> Cgc<DynObj> {
        let mut attrs = vec![];

        for _ in 0..attrs_len {
            let obj = stack.pop().unwrap();
            attrs.push(obj.clone());
        }

        Cgc::new(DynObj::Ist(InstanceObj {
            cls: cls_idx,
            attrs: attrs
        }))
    }
}

impl Object for InstanceObj {
    /// We can't invoke internally because the class's methods will
    /// be checked first.
    fn invoke(&self, name: &str, _: &mut Vec<Cgc<DynObj>>, _: &Env) {
        invoke_fail("InstanceObj", name)
    }

    fn get(&self, name: &str, env: &Env) -> Cgc<DynObj> {
        let parent: &Class = &env.classes[self.cls as usize];

        for i in 0..parent.attrs.len() {
            if parent.attrs[i] == name.to_string() {
                return self.attrs[i].clone();
            }
        }

        access_fail("InstanceObj", name)
    }

    fn set(&mut self, name: &str, new_obj: &Cgc<DynObj>, env: &Env) {
        let parent: &Class = &env.classes[self.cls as usize];

        for i in 0..parent.attrs.len() {
            if parent.attrs[i] == name.to_string() {
                self.attrs[i] = new_obj.clone();
            }
        }

        access_fail("InstanceObj", name);
    }

    fn to_string(&self) -> String {
        "<instance>".to_string()
    }

    fn tyof(&self) -> String {
        "<instance>".to_string()
    }
}
