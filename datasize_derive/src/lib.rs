//! Proc-macro `DataSize` derive for use with the `datasize` crate.

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DataStruct, DeriveInput, Ident};

/// Automatically derive the `DataSize` trait for a struct.
///
/// Supports a single option, `#[datasize(skip)]`. If set on a field, it will be ignored entirely
/// when deriving the implementation.
#[proc_macro_derive(DataSize, attributes(data_size))]
pub fn derive_data_size(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match input.data {
        syn::Data::Struct(ds) => derive_for_struct(input.ident, ds),
        syn::Data::Enum(de) => panic!("enums not supported"),
        syn::Data::Union(_) => panic!("unions not supported"),
    }
}

fn derive_for_struct(name: Ident, ds: DataStruct) -> TokenStream {
    let fields = ds.fields;

    let mut is_dynamic = proc_macro2::TokenStream::new();
    let mut static_heap_size = proc_macro2::TokenStream::new();
    let mut dynamic_size = proc_macro2::TokenStream::new();

    'outer: for field in fields.iter() {
        for attr in &field.attrs {
            let parsed: Ident = attr
                .parse_args()
                .expect("could not parse datasize attribute");

            // Quick hack to save us from writing an actual parser. This will need to rewritten once
            // we add additional attributes.
            match parsed.to_string().as_str() {
                "skip" => continue 'outer,
                s => panic!(format!("unexpected datasize attribute {}", s)),
            }
        }

        if !is_dynamic.is_empty() {
            is_dynamic.extend(quote!(|));
        }

        if !static_heap_size.is_empty() {
            static_heap_size.extend(quote!(+));
        }

        if !dynamic_size.is_empty() {
            dynamic_size.extend(quote!(+));
        }

        let ty = &field.ty;
        is_dynamic.extend(quote!(<#ty as datasize::DataSize>));
        is_dynamic.extend(quote!(::IS_DYNAMIC));

        static_heap_size.extend(quote!(<#ty as datasize::DataSize>));
        static_heap_size.extend(quote!(::STATIC_HEAP_SIZE));

        let handle = if let Some(ref ident) = &field.ident {
            ident
        } else {
            todo!();
        };

        dynamic_size.extend(quote!(
            datasize::data_size::<#ty>(&self.#handle)
        ));
    }

    TokenStream::from(quote! {
        impl datasize::DataSize for #name {
            const IS_DYNAMIC: bool = #is_dynamic;
            const STATIC_HEAP_SIZE: usize = #static_heap_size;

            fn estimate_heap_size(&self) -> usize {
                #dynamic_size
            }
        }
    })
}
