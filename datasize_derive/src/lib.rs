extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro_derive(DataSize)]
pub fn derive_data_size(_item: TokenStream) -> TokenStream {
    "fn answer() -> u32 { 42 }".parse().unwrap()
}
