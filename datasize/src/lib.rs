//! Heap data estimator.
//!
//! The `datasize` crate allows estimating the amount of heap memory used by a value. It does so by
//! providing or deriving an implementation of the `DataSize` trait, which knows how to calculate
//! the size for many `std` types and primitives.
//!
//! The aim is to get a reasonable approximation of memory usage, especially with variably sized
//! types like `Vec`s. While it is acceptable to be a few bytes off in some cases, any user should
//! be able to easily tell whether their memory is growing linearly or logarithmically by glancing
//! at the reported numbers.
//!
//! The crate does not take alignment or memory layouts into account, or unusual behavior or
//! optimizations of allocators. It is depending entirely on the data inside the type, thus the name
//! of the crate.
//!
//! # General usage
//!
//! For any type that implements `DataSize`, the `data_size` convenience function can be used to
//! guess the size of its heap allocation:
//!
//! ```rust
//! use datasize::data_size;
//!
//! let data: Vec<u64> = vec![1, 2, 3];
//! assert_eq!(data_size(&data), 24);
//! ```
//!
//! Types implementing the trait also provide two additional constants, `IS_DYNAMIC` and
//! `STATIC_HEAP_SIZE`.
//!
//! `IS_DYNAMIC` indicates whether a value's size can change over time:
//!
//! ```rust
//! use datasize::DataSize;
//!
//! // A `Vec` of any kind may have elements added or removed, so it changes size.
//! assert!(Vec::<u64>::IS_DYNAMIC);
//!
//! // The elements of type `u64` in it are not dynamic. This allows the implementation to
//! // simply estimate the size as number_of_elements * size_of::<u64>.
//! assert!(!u64::IS_DYNAMIC);
//! ```
//!
//! Additionally, `STATIC_HEAP_SIZE` indicates the amount of heap memory a type will always use. A
//! good example is a `Box<u64>` -- it will always use 8 bytes of heap memory, but not change in
//! size:
//!
//!
//! ```rust
//! use datasize::DataSize;
//!
//! assert_eq!(Box::<u64>::STATIC_HEAP_SIZE, 8);
//! assert!(!Box::<u64>::IS_DYNAMIC);
//! ```
//!
//! # Implementing `DataSize` for custom types
//!
//! The `DataSize` trait can be implemented for custom types manually:
//!
//! ```rust
//! # use datasize::{DataSize, data_size};
//! struct MyType {
//!     items: Vec<i64>,
//!     flag: bool,
//!     counter: Box<u64>,
//! }
//!
//! impl DataSize for MyType {
//!     // `MyType` contains a `Vec`, so `IS_DYNAMIC` is set to true.
//!     const IS_DYNAMIC: bool = true;
//!
//!     // The only always present heap item is the `counter` value, which is 8 bytes.
//!     const STATIC_HEAP_SIZE: usize = 8;
//!
//!     #[inline]
//!     fn estimate_heap_size(&self) -> usize {
//!         // We can be lazy here and delegate to all the existing implementations:
//!         data_size(&self.items) + data_size(&self.flag) + data_size(&self.counter)
//!     }
//! }
//!
//! let my_data = MyType {
//!     items: vec![1, 2, 3],
//!     flag: true,
//!     counter: Box::new(42),
//! };
//!
//! // Three i64 and one u64 on the heap sum up to 32 bytes:
//! assert_eq!(data_size(&my_data), 32);
//! ```
//!
//! Since implementing this for `struct` types is cumbersome and repetitive, the crate provides a
//! `DataSize` macro for convenience:
//!
//! ```
//! # use datasize::{DataSize, data_size};
//! // Equivalent to the manual implementation above:
//! #[derive(DataSize)]
//! struct MyType {
//!     items: Vec<i64>,
//!     flag: bool,
//!     counter: Box<u64>,
//! }
//! # let my_data = MyType {
//! #     items: vec![1, 2, 3],
//! #     flag: true,
//! #     counter: Box::new(42),
//! # };
//! # assert_eq!(data_size(&my_data), 32);
//! ```
//!
//! See the `DataSize` macro documentation in the `datasize_derive` crate for details.
//!
//! ## Performance considerations
//!
//! Determining the full size of data can be quite expensive, especially if multiple nested levels
//! of dynamic types are used. The crate uses `IS_DYNAMIC` and `STATIC_HEAP_SIZE` to optimize when
//! it can, so in many cases not every element of a vector needs to be checked individually.
//!
//! However, if the contained types are dynamic, every element must (and will) be checked, so keep
//! this in mind when performance is an issue.

pub use datasize_derive::DataSize;
use std::mem::size_of;

/// Indicates that a type knows how to approximate its memory usage.
pub trait DataSize {
    /// If `true`, the type has a heap size that can vary at runtime, depending on the actual value.
    const IS_DYNAMIC: bool;

    /// The amount of space a value of the type _always_ occupies. If `IS_DYNAMIC` is false, this is
    /// the total amount of heap memory occupied by the value. Otherwise this is a lower bound.
    const STATIC_HEAP_SIZE: usize;

    /// Estimates the size of heap memory taken up by this value.
    ///
    /// Does not include data on the stack, which is usually determined using `mem::size_of`.
    fn estimate_heap_size(&self) -> usize;
}

/// Estimates allocated heap data from data of value.
///
/// Checks if `T` is dynamic; if it is not, returns `T::STATIC_HEAP_SIZE`. Otherwise delegates to
/// `T::estimate_heap_size`.
#[inline]
pub fn data_size<T>(value: &T) -> usize
where
    T: DataSize,
{
    if T::IS_DYNAMIC {
        // The type changes at runtime, so we always have to query the instance.
        value.estimate_heap_size()
    } else {
        // Type does not change at runtime, so it will only occupy the `STATIC_HEAP_SIZE`.
        T::STATIC_HEAP_SIZE
    }
}

macro_rules! non_dynamic_const_heap_size {
    ($($ty:ty)*, $sz:expr) => {
        $(impl DataSize for $ty {
            const IS_DYNAMIC: bool = false;
            const STATIC_HEAP_SIZE: usize = $sz;

            #[inline]
            fn estimate_heap_size(&self) -> usize {
                $sz
            }
        })*
    };
}

// Hack to allow `+` to be used to join macro arguments.
macro_rules! strip_plus {
    (+ $($rest: tt)*) => {
        $($rest)*
    }
}

macro_rules! tuple_heap_size {
    ($($n:tt $name:ident);+) => {
        impl<$($name),*> DataSize for ($($name),*)
        where $($name: DataSize),*
        {
            const IS_DYNAMIC: bool = $($name::IS_DYNAMIC)|*;

            const STATIC_HEAP_SIZE: usize =
                strip_plus!($(+ $name::STATIC_HEAP_SIZE)+);

            #[inline]
            fn estimate_heap_size(&self) -> usize {
                strip_plus!($(+ self.$n.estimate_heap_size())+)
            }
        }
    };
}

macro_rules! array_heap_size {
    ($($n:tt)+) => {
        $(
        impl<T> DataSize for [T; $n]
        where
            T: DataSize,
        {
            const IS_DYNAMIC: bool = T::IS_DYNAMIC;

            const STATIC_HEAP_SIZE: usize = T::STATIC_HEAP_SIZE * $n;

            #[inline]
            fn estimate_heap_size(&self) -> usize {
                if T::IS_DYNAMIC {
                    (&self[..]).iter().map(DataSize::estimate_heap_size).sum()
                } else {
                    T::STATIC_HEAP_SIZE * $n
                }
            }
        }
        )*
    };
}

non_dynamic_const_heap_size!(() u8 u16 u32 u64 u128 usize i8 i16 i32 i64 i128 isize bool char, 0);

tuple_heap_size!(0 T0; 1 T1);
tuple_heap_size!(0 T0; 1 T1; 2 T2);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7; 8 T8);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7; 8 T8; 9 T9);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7; 8 T8; 9 T9; 10 T10);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7; 8 T8; 9 T9; 10 T10; 11 T11);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7; 8 T8; 9 T9; 10 T10; 11 T11; 12 T12);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7; 8 T8; 9 T9; 10 T10; 11 T11; 12 T12; 13 T13);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7; 8 T8; 9 T9; 10 T10; 11 T11; 12 T12; 13 T13; 14 T14);
tuple_heap_size!(0 T0; 1 T1; 2 T2; 3 T3; 4 T4; 5 T5; 6 T6; 7 T7; 8 T8; 9 T9; 10 T10; 11 T11; 12 T12; 13 T13; 14 T14; 15 T15);

array_heap_size!(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 128 192 256 384 512 1024 2048 4096 8192 16384 1048576 2097152 3145728 4194304);

// COMMONLY USED NON-PRIMITIVE TYPES

impl<T> DataSize for Box<T>
where
    T: DataSize,
{
    const IS_DYNAMIC: bool = T::IS_DYNAMIC;

    const STATIC_HEAP_SIZE: usize = size_of::<T>();

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        // Total size is the struct itself + its children.
        size_of::<T>() + data_size::<T>(self)
    }
}

impl<T> DataSize for Option<T>
where
    T: DataSize,
{
    // Options are only not dynamic if their type has no heap data at all and is not dynamic.
    const IS_DYNAMIC: bool = (T::IS_DYNAMIC || T::STATIC_HEAP_SIZE > 0);

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        match self {
            Some(val) => data_size(val),
            None => 0,
        }
    }
}

// CONTAINERS

impl<T> DataSize for Vec<T>
where
    T: DataSize,
{
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        // We do not include the `STATIC_HEAP_SIZE`, since the heap data has not been allocated yet.
        let sz_base = self.capacity() * size_of::<T>();

        let sz_used = if T::IS_DYNAMIC {
            self.iter().map(DataSize::estimate_heap_size).sum()
        } else {
            self.len() * T::STATIC_HEAP_SIZE
        };

        sz_base + sz_used
    }
}

impl DataSize for String {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        self.capacity()
    }
}

impl DataSize for std::path::PathBuf {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        self.capacity()
    }
}

impl DataSize for std::ffi::OsString {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        self.capacity()
    }
}

#[cfg(test)]
mod tests {
    use crate as datasize; // Required for the derive macro.
    use crate::{data_size, DataSize};

    #[test]
    fn test_for_simple_builtin_types() {
        // We only sample some, as they are all macro generated.
        assert_eq!(1u8.estimate_heap_size(), 0);
        assert_eq!(1u16.estimate_heap_size(), 0);
    }

    #[test]
    fn test_box() {
        let value: Box<u64> = Box::new(1234);

        assert_eq!(data_size::<Box<u64>>(&value), 8);
        assert_eq!(data_size(&value), 8);
    }

    #[test]
    fn test_option_box() {
        let value_none: Option<Box<u64>> = None;
        let value_some: Option<Box<u64>> = Some(Box::new(12345));

        assert_eq!(data_size::<Option<Box<u64>>>(&value_none), 0);
        assert_eq!(data_size::<Option<Box<u64>>>(&value_some), 8);
    }

    #[test]
    fn test_struct() {
        #[derive(DataSize)]
        struct Example {
            count: usize,
            my_data: Vec<MyStruct>,
            warning: Option<Box<u32>>,
            #[data_size(skip)]
            #[allow(dead_code)]
            skipped: Box<char>,
        }

        #[derive(DataSize)]
        struct MyStruct {
            count: u64,
        }

        // Start with a small example struct.
        let mut ex = Example {
            count: 99,
            my_data: vec![],
            warning: None,
            skipped: Default::default(),
        };

        // We expect a heap size of 0, as the vec is empty and no box allocated.
        assert_eq!(data_size(&ex), 0);

        // Add a `warning` will cause a heap allocation.
        ex.warning = Some(Box::new(12345));
        assert_eq!(data_size(&ex), 4);

        // Let's reserve some capacity on `my_data`.
        ex.my_data.reserve_exact(10);
        assert_eq!(data_size(&ex), 4 + 10 * 8)
    }

    #[test]
    fn test_enum() {
        #[derive(Debug, DataSize)]
        enum Foo {
            Bar,
            Baz {
                boxed: Box<u32>,
                nonheap: u8,
                #[data_size(skip)]
                extra: Box<u128>,
            },
            Bert(Vec<u32>, #[data_size(skip)] Vec<u8>),
            #[data_size(skip)]
            Skipped(Vec<i32>),
        }

        let bar = Foo::Bar;
        assert_eq!(data_size(&bar), 0);

        let baz = Foo::Baz {
            boxed: Box::new(123),
            nonheap: 99,
            extra: Box::new(456),
        };
        assert_eq!(data_size(&baz), 4);

        let bert = Foo::Bert(vec![5, 6, 7, 8, 9], vec![1, 2, 3, 4, 5]);
        assert_eq!(data_size(&bert), 5 * 4);

        let skipped = Foo::Skipped(vec![-1, 1, 99, 100]);
        assert_eq!(data_size(&skipped), 0);
    }

    #[test]
    fn test_generic_struct() {
        #[derive(DataSize)]
        struct Example<A, B> {
            a: Option<A>,
            b: Option<B>,
            c: u8,
        }

        let none: Example<Box<u32>, Box<u8>> = Example {
            a: None,
            b: None,
            c: 123,
        };
        assert_eq!(data_size(&none), 0);

        let a: Example<Box<u32>, Box<u8>> = Example {
            a: Some(Box::new(0)),
            b: None,
            c: 123,
        };
        assert_eq!(data_size(&a), 4);

        let both: Example<Box<u32>, Box<u8>> = Example {
            a: Some(Box::new(0)),
            b: Some(Box::new(0)),
            c: 123,
        };
        assert_eq!(data_size(&both), 5);
    }

    #[test]
    fn test_generic_enum() {
        #[derive(DataSize)]
        enum Foo<A, B, C, D> {
            Baz {
                boxed: Box<A>,
                #[data_size(skip)]
                #[allow(dead_code)]
                extra: Box<B>,
            },
            Bert(Vec<A>, #[data_size(skip)] Vec<D>, Box<A>),
            #[data_size(skip)]
            Skipped(Vec<C>),
        }

        let baz: Foo<u8, u16, u32, u64> = Foo::Baz {
            boxed: Box::new(123),
            extra: Box::new(456),
        };
        assert_eq!(data_size(&baz), 1);

        let bert: Foo<u8, u16, u32, u64> =
            Foo::Bert(vec![5, 6, 7, 8, 9], vec![1, 2, 3, 4, 5], Box::new(1));
        assert_eq!(data_size(&bert), 5 + 1);

        let skipped: Foo<u8, u16, u32, u64> = Foo::Skipped(vec![1, 1, 99, 100]);
        assert_eq!(data_size(&skipped), 0);
    }

    #[test]
    fn test_empty_struct() {
        #[derive(DataSize)]
        struct Foo {}

        #[derive(DataSize)]
        struct Bar;

        assert!(!Foo::IS_DYNAMIC);
        assert!(!Bar::IS_DYNAMIC);

        assert_eq!(Foo::STATIC_HEAP_SIZE, 0);
        assert_eq!(Bar::STATIC_HEAP_SIZE, 0);

        assert_eq!(data_size(&Foo {}), 0);
        assert_eq!(data_size(&Bar), 0);
    }

    #[test]
    fn test_empty_enum() {
        #[derive(DataSize)]
        enum Foo {}

        assert!(!Foo::IS_DYNAMIC);
        assert_eq!(Foo::STATIC_HEAP_SIZE, 0);

        // We cannot instantiate empty enums.
    }

    #[test]
    fn macro_does_not_panic_on_foreign_attributes() {
        #[derive(DataSize)]
        /// This docstring shows up as `#[doc = ""]`...
        struct Foo {
            /// This docstring shows up as `#[doc = ""]`...
            dummy: u8,
        }
    }

    // TODO: This does not work, the equivalent should be constructed using `trybuild`.
    // #[test]
    // #[should_panic = "unexpected datasize attribute"]
    // fn macro_panics_on_invalid_data_size_attribute() {
    //     #[derive(DataSize)]
    //     /// This docstring shows up as `#[doc = ""]`...
    //     struct Foo {
    //         #[data_size(invalid)]
    //         /// This docstring shows up as `#[doc = ""]`...
    //         dummy: u8,
    //     }
    // }
}
