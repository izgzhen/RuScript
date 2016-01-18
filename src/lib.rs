#![crate_name = "ruscript"]

#![feature(plugin, custom_derive, custom_attribute, box_syntax, test)]
#![plugin(gc_plugin)]
extern crate gc;
extern crate test;

// ---- Global definitions ----- //

pub type Integer = i32;

// ---- Public modules ---- //

pub mod bytecode;
pub mod deserialize;
pub mod class;
pub mod env;
pub mod object;
pub mod instance;
pub mod primitives;
pub mod dispatch;