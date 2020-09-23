use std::mem::size_of;

/// Indicates that a type knows how to approximate its memory usage.
pub trait DataSizeOf {
    /// If `true`, the type has a heap size that can vary at runtime, depending on the actual value
    /// of the type.
    const IS_DYNAMIC: bool;

    /// The amount of space a value of the type _always_ occupies. If `IS_DYNAMIC` is false, this is
    /// the total amount of heap memory occupied by the value. Otherwise this is a lower bound.
    const STATIC_HEAP_SIZE: usize;

    /// Estimate the size of heap memory taken up by this value.
    ///
    /// Does not include data on the stack, use `mem::size_of` to determine it.
    fn estimate_heap_size(&self) -> usize;
}

#[inline]
pub fn data_size<T>(value: &T) -> usize
where
    T: DataSizeOf,
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
        $(impl DataSizeOf for $ty {
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
        impl<$($name),*> DataSizeOf for ($($name),*)
        where $($name: DataSizeOf),*
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
        impl<T> DataSizeOf for [T; $n]
        where
            T: DataSizeOf,
        {
            const IS_DYNAMIC: bool = T::IS_DYNAMIC;

            const STATIC_HEAP_SIZE: usize = T::STATIC_HEAP_SIZE * $n;

            #[inline]
            fn estimate_heap_size(&self) -> usize {
                if T::IS_DYNAMIC {
                    (&self[..]).iter().map(DataSizeOf::estimate_heap_size).sum()
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

impl<T> DataSizeOf for Box<T>
where
    T: DataSizeOf,
{
    const IS_DYNAMIC: bool = T::IS_DYNAMIC;

    const STATIC_HEAP_SIZE: usize = size_of::<T>();

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        // Total size is the struct itself + its children.
        size_of::<T>() + data_size::<T>(self)
    }
}

impl<T> DataSizeOf for Option<T>
where
    T: DataSizeOf,
{
    const IS_DYNAMIC: bool = T::IS_DYNAMIC;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        match self {
            Some(val) => data_size(val),
            None => 0,
        }
    }
}

// CONTAINERS

impl<T> DataSizeOf for Vec<T>
where
    T: DataSizeOf,
{
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        // We do not include the `STATIC_HEAP_SIZE`, since the heap data has not been allocated yet.
        let sz_base = self.capacity() * size_of::<T>();

        let sz_used = if T::IS_DYNAMIC {
            self.iter().map(DataSizeOf::estimate_heap_size).sum()
        } else {
            self.len() * T::STATIC_HEAP_SIZE
        };

        sz_base + sz_used
    }
}

#[cfg(test)]
mod tests {
    use crate::DataSizeOf;

    #[test]
    fn test_for_simple_builtin_types() {
        // We only sample some, as they are all macro generated.
        assert_eq!(1u8.estimate_heap_size(), 0);
        assert_eq!(1u16.estimate_heap_size(), 0);
    }
}
