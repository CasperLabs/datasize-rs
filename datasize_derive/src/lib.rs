//! Proc-macro `DataSize` derive for use with the `datasize` crate.

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, AttrStyle, Attribute, DataEnum, DataStruct, DeriveInput, Generics, Ident,
};

/// Automatically derive the `DataSize` trait for a type.
///
/// Supports a single option, `#[datasize(skip)]`. If set on a field, it will be ignored entirely
/// when deriving the implementation.
#[proc_macro_derive(DataSize, attributes(data_size))]
pub fn derive_data_size(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    match input.data {
        syn::Data::Struct(ds) => derive_for_struct(input.ident, input.generics, ds),
        syn::Data::Enum(de) => derive_for_enum(input.ident, input.generics, de),
        syn::Data::Union(_) => panic!("unions not supported"),
    }
}

/// Determines if attributes contain `#[data_size(skip)]`.
fn should_skip(attrs: &Vec<Attribute>) -> bool {
    for attr in attrs {
        if attr.style != AttrStyle::Outer {
            // We ignore out attributes.
            continue;
        }

        // Ensure it is a `data_size` attribute.
        if attr.path.segments.len() != 1 || attr.path.segments[0].ident.to_string() != "data_size" {
            continue;
        }

        let parsed: Ident = attr
            .parse_args()
            .expect("could not parse datasize attribute");

        match parsed.to_string().as_str() {
            "skip" => return true,
            s => panic!(format!("unexpected datasize attribute {}", s)),
        }
    }

    false
}

/// Derives `DataSize` for a `struct`
fn derive_for_struct(name: Ident, generics: Generics, ds: DataStruct) -> TokenStream {
    let fields = ds.fields;

    let mut where_clauses = proc_macro2::TokenStream::new();
    let mut is_dynamic = proc_macro2::TokenStream::new();
    let mut static_heap_size = proc_macro2::TokenStream::new();
    let mut dynamic_size = proc_macro2::TokenStream::new();

    for field in fields.iter().filter(|f| !should_skip(&f.attrs)) {
        // We need a where clause for every non-skipped field. It is harmless to add the type
        // constraint for fields that do not have generic arguments though.
        if where_clauses.is_empty() {
            where_clauses.extend(quote!(where));
        }

        let ty = &field.ty;

        where_clauses.extend(quote!(
            #ty : datasize::DataSize,
        ));

        if !is_dynamic.is_empty() {
            is_dynamic.extend(quote!(|));
        }

        if !static_heap_size.is_empty() {
            static_heap_size.extend(quote!(+));
        }

        if !dynamic_size.is_empty() {
            dynamic_size.extend(quote!(+));
        }

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

    // Handle structs with no fields.
    if is_dynamic.is_empty() {
        is_dynamic.extend(quote!(false));
    }
    if static_heap_size.is_empty() {
        static_heap_size.extend(quote!(0));
    }
    if dynamic_size.is_empty() {
        dynamic_size.extend(quote!(0));
    }

    TokenStream::from(quote! {
        impl #generics datasize::DataSize for #name #generics #where_clauses {
            const IS_DYNAMIC: bool = #is_dynamic;
            const STATIC_HEAP_SIZE: usize = #static_heap_size;

            fn estimate_heap_size(&self) -> usize {
                #dynamic_size
            }
        }
    })
}

/// Derives `DataSize` for an `enum`
fn derive_for_enum(name: Ident, generics: Generics, de: DataEnum) -> TokenStream {
    let mut match_arms = proc_macro2::TokenStream::new();
    let mut where_types = proc_macro2::TokenStream::new();

    let mut skipped = false;
    for variant in de.variants.into_iter() {
        if should_skip(&variant.attrs) {
            skipped = true;
            continue;
        }

        let variant_ident = variant.ident;

        let mut field_match = proc_macro2::TokenStream::new();
        let mut field_calc = proc_macro2::TokenStream::new();

        match variant.fields {
            syn::Fields::Named(fields) => {
                let mut left = proc_macro2::TokenStream::new();

                for field in fields.named.into_iter() {
                    let ident = field.ident.expect("named fields must have idents");
                    let skip = should_skip(&field.attrs);

                    if skip {
                        left.extend(quote!(#ident:_));
                    } else {
                        left.extend(quote!(#ident ,));

                        let ty = field.ty;
                        where_types.extend(quote!(#ty : datasize::DataSize,));
                    }

                    if !skip {
                        if !field_calc.is_empty() {
                            field_calc.extend(quote!(+));
                        }
                        field_calc.extend(quote!(DataSize::estimate_heap_size(#ident)));
                    }
                }

                field_match.extend(quote! {
                    {#left}
                });
            }
            syn::Fields::Unnamed(fields) => {
                let mut left = proc_macro2::TokenStream::new();

                for (idx, field) in fields.unnamed.into_iter().enumerate() {
                    let skip = should_skip(&field.attrs);
                    let ident = Ident::new(
                        &format!("{}f{}", if skip { "_" } else { "" }, idx),
                        proc_macro2::Span::call_site(),
                    );

                    left.extend(quote!(#ident ,));

                    if !skip {
                        if !field_calc.is_empty() {
                            field_calc.extend(quote!(+));
                        }
                        field_calc.extend(quote!(DataSize::estimate_heap_size(#ident)));

                        let ty = field.ty;
                        where_types.extend(quote!(#ty : datasize::DataSize,));
                    }
                }

                field_match.extend(quote! {
                    (#left)
                });
            }
            syn::Fields::Unit => {
                field_calc.extend(quote!(0));
            }
        }

        match_arms.extend(quote!(
            #name::#variant_ident #field_match => { #field_calc }
        ));
    }

    // If we skipped any variant, add a fallback.
    if skipped {
        match_arms.extend(quote! {
            _ => 0,
        })
    }

    let mut where_clause = proc_macro2::TokenStream::new();
    if !where_types.is_empty() {
        where_clause.extend(quote!(where #where_types));
    }

    TokenStream::from(quote! {
        impl #generics DataSize for #name #generics #where_clause {
            // TODO: Accurately determine `IS_DYNAMIC` and `STATIC_HEAP_SIZE`.
            //
            //       It is possible to accurately pre-calculate these, but it takes a bit of extra
            //       effort. `IS_DYNAMIC` depends on none of the variants (and their fields) being
            //       being dynamic, as well as all having the same `STATIC_HEAP_SIZE`.
            //
            //       `STATIC_HEAP_SIZE` in turn is the minimum of `STATIC_HEAP_SIZE` of every
            //       variant (which are the sum of their fields). `min` can be determiend by the
            //       following `const fn`:
            //
            //           const fn min(a: usize, b: usize) -> usize {
            //               [a, b][(a > b) as usize]
            //           }

            const IS_DYNAMIC: bool = true;
            const STATIC_HEAP_SIZE: usize = 0;

            #[inline]
            fn estimate_heap_size(&self) -> usize {
                match self {
                    #match_arms
                }
            }
        }
    })
}
