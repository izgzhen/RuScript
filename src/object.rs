/* 
 * Object trait interface
 *
 * Every thing allocated in heap, traced by GC,
 * and referenced through local variable and global
 * stack should implement this trait interface
 * 
 */

use gc::*;
use std::fmt::Debug;
use env::Env;
use dispatch::*;

pub trait Object : Trace {
    fn invoke(&mut self, name: &str, args: Vec<Gc<DynObj>>, env: &Env) -> Option<Gc<DynObj>>;

    fn get(&self, name: &str, env: &Env) -> Gc<DynObj> {
        access_fail(&self.tyof(), name)
    }

    fn set(&mut self, name: &str, new: &Gc<DynObj>, env: &Env) {
        access_fail(&self.tyof(), name);
    }

    fn tyof(&self) -> String;
}

pub fn invoke_fail(ty: &str, name: &str) -> Option<Gc<DynObj>> {
    panic!("{:?} has no such method {:?}", ty, name)
}

pub fn access_fail<N>(ty: &str, name: N) -> Gc<DynObj> where N: Debug {
    panic!("{:?} has no such attr {:?}", ty, name)
}
