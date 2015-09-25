#![crate_name = "ruscript"]

#![feature(plugin, custom_derive, custom_attribute)]
#![feature(box_syntax)]
#![feature(test)]
#![plugin(gc_plugin)]
extern crate gc;
extern crate test;

pub mod object;


#[cfg(test)]
mod benches;
