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

pub trait Object : Trace {
    fn invoke(&mut self, name: &str, args: Vec<Gc<Object>>, env: &Env) -> Option<Gc<Object>>;

    fn get(&self, name: &str, env: &Env) -> Gc<Object>;

    fn set(&mut self, name: &str, new: &Gc<Object>, env: &Env);

    fn tyof(&self) -> String;
}

pub fn invoke_fail(ty: &str, name: &str) -> Option<Gc<Object>> {
    println!("{:?} has no such method {:?}", ty, name);
    None
}

pub fn access_fail<N>(ty: &str, name: N) -> Gc<Object> where N: Debug {
    panic!("{:?} has no such attr {:?}", ty, name)
}
