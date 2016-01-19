/* 
 * Instance object
 *
 */

use super::*;
use gc::*;
use object::*;
use class::Class;
use env::Env;
use dispatch::*;

#[derive(Trace)]
pub struct InstanceObj {
    cls   : Integer,
    attrs : Vec<Gc<DynObj>>,
}

impl Object for InstanceObj {
    fn invoke(&mut self, name: &str, args: Vec<Gc<DynObj>>, env: &Env) -> Option<Gc<DynObj>> {
        
        invoke_fail("InstanceObj", name)
    }

    fn get(&self, name: &str, env: &Env) -> Gc<DynObj> {
        let parent: &Class = &env.classes[self.cls as usize];

        for i in 0..parent.attrs.len() {
            if parent.attrs[i] == name.to_string() {
                return self.attrs[i].clone();
            }
        }

        access_fail("InstanceObj", name)
    }

    fn set(&mut self, name: &str, new_obj: &Gc<DynObj>, env: &Env) {
        let parent: &Class = &env.classes[self.cls as usize];

        for i in 0..parent.attrs.len() {
            if parent.attrs[i] == name.to_string() {
                self.attrs[i] = new_obj.clone();
            }
        }

        access_fail("InstanceObj", name);
    }

    

    fn tyof(&self) -> String {
        "<instance>".to_string()
    }
}
