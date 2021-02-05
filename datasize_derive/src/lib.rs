//! Proc-macro `DataSize` derive for use with the `datasize` crate.

use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashSet;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, AttrStyle, Attribute, Binding, DataEnum,
    DataStruct, DeriveInput, Generics, Ident, Index, ParenthesizedGenericArguments, Path,
    PathArguments, ReturnType, TraitBound, Type, TypeArray, TypeBareFn, TypeGroup, TypeImplTrait,
    TypeParam, TypeParamBound, TypeParen, TypePath, TypePtr, TypeReference, TypeSlice,
    TypeTraitObject, TypeTuple, WhereClause,
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

/// Returns whether any of the `generics` show up in `ty`.
///
/// Used to determine whether or not a specific type needs to be listed in where clauses because it
/// contains generic types. Note that the function is not entirely accurate and may produce false
/// postives in some cases.
fn contains_generic(generics: &Generics, ty: &Type) -> bool {
    match ty {
        Type::Array(TypeArray { elem, .. }) => contains_generic(generics, &elem),
        Type::BareFn(TypeBareFn { inputs, output, .. }) => {
            for arg in inputs {
                if contains_generic(&generics, &arg.ty) {
                    return true;
                }
            }

            match output {
                ReturnType::Default => false,
                ReturnType::Type(_, ty) => contains_generic(generics, ty),
            }
        }
        Type::Group(TypeGroup { elem, .. }) => contains_generic(generics, elem),
        Type::ImplTrait(TypeImplTrait { bounds, .. }) => bounds
            .iter()
            .any(|b| param_bound_contains_generic(generics, b)),
        Type::Infer(_) => false,
        Type::Macro(_) => true,
        Type::Never(_) => false,
        Type::Paren(TypeParen { elem, .. }) => contains_generic(generics, elem),
        Type::Path(TypePath { path, .. }) => path_contains_generic(generics, path),
        Type::Ptr(TypePtr { elem, .. }) => contains_generic(generics, elem),
        Type::Reference(TypeReference { elem, .. }) => contains_generic(generics, elem),
        Type::Slice(TypeSlice { elem, .. }) => contains_generic(generics, elem),
        Type::TraitObject(TypeTraitObject { bounds, .. }) => bounds
            .iter()
            .any(|b| param_bound_contains_generic(generics, b)),
        Type::Tuple(TypeTuple { elems, .. }) => {
            elems.iter().any(|ty| contains_generic(generics, ty))
        }
        // Nothing we can do here, err on the side of too many `where` clauses.
        Type::Verbatim(_) => true,
        // TODO: This may be problematic, double check if we did not miss anything.
        _ => true,
    }
}

/// Returns whether any of the `generics` show up in a given path.
///
/// May yield false positives.
fn path_contains_generic(generics: &Generics, path: &Path) -> bool {
    let mut candidates = HashSet::new();

    for segment in &path.segments {
        candidates.insert(segment.ident.clone());

        match &segment.arguments {
            PathArguments::None => {}
            PathArguments::AngleBracketed(AngleBracketedGenericArguments { ref args, .. }) => {
                for arg in args {
                    match arg {
                        syn::GenericArgument::Lifetime(_) => {
                            // Ignore lifetime args.
                        }
                        syn::GenericArgument::Type(ty) => {
                            // Simply recurse and check directly.
                            if contains_generic(generics, ty) {
                                return true;
                            }
                        }
                        syn::GenericArgument::Binding(Binding {
                            // We can ignore the ident here, as it is local.
                            ty,
                            ..
                        }) => {
                            // Again, exit early with `true` if we find a match.
                            if contains_generic(generics, ty) {
                                return true;
                            }
                        }
                        syn::GenericArgument::Constraint(_) => {
                            // Additional constraints are fine?
                        }
                        syn::GenericArgument::Const(_) => {
                            // Constants do not require `DataSize` impls.
                        }
                    }
                }
            }
            syn::PathArguments::Parenthesized(ParenthesizedGenericArguments {
                inputs,
                output,
                ..
            }) => {
                if inputs.iter().any(|ty| contains_generic(generics, ty)) {
                    return true;
                }

                match output {
                    ReturnType::Default => {}
                    ReturnType::Type(_, ref ty) => {
                        if contains_generic(generics, ty) {
                            return true;
                        }
                    }
                }
            }
        }
    }

    let generic_idents: HashSet<_> = generics
        .params
        .iter()
        .filter_map(|gen| match gen {
            syn::GenericParam::Type(TypeParam { ident, .. }) => Some(ident.clone()),
            syn::GenericParam::Lifetime(_) => None,
            syn::GenericParam::Const(_) => None,
        })
        .collect();

    // If we find at least one generic in all of the types, we return `true` here.
    candidates.intersection(&generic_idents).next().is_some()
}

/// Returns whether any of the `generics` show up in a type parameter binding.
///
/// May return false positives.
fn param_bound_contains_generic(generics: &Generics, bound: &TypeParamBound) -> bool {
    match bound {
        syn::TypeParamBound::Trait(TraitBound { path, .. }) => {
            path_contains_generic(generics, path)
        }
        syn::TypeParamBound::Lifetime(_) => false,
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
    let mut detail_calls = proc_macro2::TokenStream::new();

    for (idx, field) in fields
        .iter()
        .enumerate()
        .filter(|(_, f)| !should_skip(&f.attrs))
    {
        let ty = &field.ty;
        // We need a where clause for every non-skipped field. We try our best to filter out bounds
        // here that are not needed (e.g. `u8: DataSize`), as they can be problematic when mixing
        // `pub(super)` and `pub` visiblity restrictions.
        if contains_generic(&generics, ty) {
            if where_clauses.is_empty() {
                where_clauses.extend(quote!(where));
            }

            where_clauses.extend(quote!(
                #ty : datasize::DataSize,
            ));
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

        is_dynamic.extend(quote!(<#ty as datasize::DataSize>));
        is_dynamic.extend(quote!(::IS_DYNAMIC));

        static_heap_size.extend(quote!(<#ty as datasize::DataSize>));
        static_heap_size.extend(quote!(::STATIC_HEAP_SIZE));

        let handle = if let Some(ref ident) = &field.ident {
            quote!(#ident)
        } else {
            let idx = Index::from(idx);
            quote!(#idx)
        };

        let name = if let Some(ref ident) = &field.ident {
            ident.to_string()
        } else {
            "idx".to_string()
        };

        dynamic_size.extend(quote!(
            datasize::data_size::<#ty>(&self.#handle)
        ));

        detail_calls.extend(quote!(
            members.insert(#name, self.#handle.estimate_detailed_heap_size());
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

    // Ensure that any `where` clause on the struct itself is preserved, otherwise the impl is
    // invalid.
    if let Some(WhereClause { ref predicates, .. }) = generics.where_clause {
        where_clauses.extend(quote!(#predicates));
    }

    let detailed_impl = if cfg!(feature = "detailed") {
        quote!(
            fn estimate_detailed_heap_size(&self) -> datasize::MemUsageNode {
                let mut members = ::std::collections::HashMap::new();
                #detail_calls
                datasize::MemUsageNode::Detailed(members)
            }
        )
    } else {
        quote!()
    };

    TokenStream::from(quote! {
        impl #generics datasize::DataSize for #name #generics #where_clauses {
            const IS_DYNAMIC: bool = #is_dynamic;
            const STATIC_HEAP_SIZE: usize = #static_heap_size;

            fn estimate_heap_size(&self) -> usize {
                #dynamic_size
            }

            #detailed_impl
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
                        if contains_generic(&generics, &ty) {
                            where_types.extend(quote!(#ty : datasize::DataSize,));
                        }
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

    // TODO: Accurately determine `IS_DYNAMIC` and `STATIC_HEAP_SIZE`.
    //
    //       It is possible to accurately pre-calculate these, but it takes a bit of extra
    //       effort. `IS_DYNAMIC` depends on none of the variants (and their fields) being
    //       being dynamic, as well as all having the same `STATIC_HEAP_SIZE`.
    //
    //       `STATIC_HEAP_SIZE` in turn is the minimum of `STATIC_HEAP_SIZE` of every
    //       variant (which are the sum of their fields). `min` can be determined by the
    //       `datasize::min` function, which is a `const fn` variant of `min`.
    let mut is_dynamic = true;
    let static_heap_size = 0usize;

    // Handle enums with no fields.
    if match_arms.is_empty() {
        match_arms.extend(quote!(_ => 0));
        is_dynamic = false;
    }

    // Ensure that any `where` clause on the struct enum is preserved.
    if let Some(WhereClause { ref predicates, .. }) = generics.where_clause {
        where_clause.extend(quote!(#predicates));
    }

    TokenStream::from(quote! {
        impl #generics DataSize for #name #generics #where_clause {

            const IS_DYNAMIC: bool = #is_dynamic;
            const STATIC_HEAP_SIZE: usize = #static_heap_size;

            #[inline]
            fn estimate_heap_size(&self) -> usize {
                match self {
                    #match_arms
                }
            }
        }
    })
}
