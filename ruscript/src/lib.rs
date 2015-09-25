#![crate_name = "ruscript"]

#![feature(plugin, custom_derive, custom_attribute)]
#![feature(box_syntax)]
#![feature(test)]
#![plugin(gc_plugin)]
extern crate gc;
extern crate test;

use gc::*;

pub type ObjIdentTy = i32;
pub type ArgIdentTy = i8;

#[allow(non_camel_case_types)]
pub type int = i64;

pub mod object;
use object::*;

pub trait Object {
    fn call(&self, &str, Vec<Gc<_Object>>) -> Gc<_Object>;
    fn tyof(&self) -> &str;
}

pub mod stackcode;
pub mod classty;
pub mod framety;
pub mod primty;

#[cfg(test)]
mod benches;

