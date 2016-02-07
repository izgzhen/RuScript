//!
//! Object trait interface
//!
//! Every thing allocated in heap, traced by GC,
//! and referenced through local variable and global
//! stack should implement this trait interface
//! 
//!

use gc::*;
use std::fmt::Debug;
use env::Env;
use dispatch::*;

pub trait Object : Trace {
    #[allow(unused_variables)]
    fn invoke(&self, name: &str, stack: &mut Vec<Gc<DynObj>>, env: &Env);

    #[allow(unused_variables)]
    fn get(&self, name: &str, env: &Env) -> Gc<DynObj> {
        access_fail(&self.tyof(), name)
    }

    #[allow(unused_variables)]
    fn set(&mut self, name: &str, new: &Gc<DynObj>, env: &Env) {
        access_fail(&self.tyof(), name);
    }

    fn to_string(&self) -> String;

    fn tyof(&self) -> String;
}

pub fn invoke_fail(ty: &str, name: &str) {
    panic!("{:?} has no such method {:?}", ty, name)
}

pub fn access_fail<N>(ty: &str, name: N) -> Gc<DynObj> where N: Debug {
    panic!("{:?} has no such attr {:?}", ty, name)
}
