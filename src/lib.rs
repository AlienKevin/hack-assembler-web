mod utils;
extern crate hack_assembler;

use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
pub fn assemble(source: &str) -> String {
    match hack_assembler::assemble(source) {
        Ok(output) => "Ok:".to_owned() + &output,
        Err(message) => "Err:".to_owned() + &message,
    }
}
