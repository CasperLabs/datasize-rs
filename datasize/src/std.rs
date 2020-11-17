use super::{data_size, non_dynamic_const_heap_size, DataSize};

use core::mem::size_of;

use std::{boxed::Box, string::String, vec::Vec};

non_dynamic_const_heap_size!(
  std::net::Ipv4Addr
  std::net::Ipv6Addr
  std::net::SocketAddrV4
  std::net::SocketAddrV6
  std::net::IpAddr
  std::net::SocketAddr

  std::time::Instant
  std::time::SystemTime,
  0
);

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

// Please see the notes in the module docs on why Arcs are not counted.
impl<T> DataSize for std::sync::Arc<T> {
    const IS_DYNAMIC: bool = false;
    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        0
    }
}

impl<T> DataSize for std::rc::Rc<T> {
    const IS_DYNAMIC: bool = false;
    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        0
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

impl<T> DataSize for std::collections::VecDeque<T>
where
    T: DataSize,
{
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        // We can treat a `VecDeque` exactly the same as a `Vec`.
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

impl<K, V> DataSize for std::collections::BTreeMap<K, V>
where
    K: DataSize,
    V: DataSize,
{
    // Approximation directly taken from
    // https://github.com/servo/heapsize/blob/f565dda63cc12c2a088bc9974a1b584cddec4382/src/lib.rs#L295-L306

    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    fn estimate_heap_size(&self) -> usize {
        let mut size = 0;

        if K::IS_DYNAMIC || V::IS_DYNAMIC {
            for (key, value) in self.iter() {
                size += size_of::<(K, V)>() + key.estimate_heap_size() + value.estimate_heap_size();
            }
        } else {
            size += self.len() * (size_of::<(K, V)>() + K::STATIC_HEAP_SIZE + V::STATIC_HEAP_SIZE);
        }
        size
    }
}

impl<K, V, S> DataSize for std::collections::HashMap<K, V, S>
where
    K: DataSize,
    V: DataSize,
{
    // Approximation directly taken from
    // https://github.com/servo/heapsize/blob/f565dda63cc12c2a088bc9974a1b584cddec4382/src/lib.rs#L266-L275
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        let size = self.capacity() * (size_of::<V>() + size_of::<K>() + size_of::<usize>());

        if K::IS_DYNAMIC || V::IS_DYNAMIC {
            self.iter().fold(size, |n, (key, value)| {
                n + key.estimate_heap_size() + value.estimate_heap_size()
            })
        } else {
            size + self.capacity() * (K::STATIC_HEAP_SIZE + V::STATIC_HEAP_SIZE)
        }
    }
}

impl<T, S> DataSize for std::collections::HashSet<T, S>
where
    T: DataSize,
{
    // Approximation directly taken from
    // https://github.com/servo/heapsize/blob/f565dda63cc12c2a088bc9974a1b584cddec4382/src/lib.rs#L255-L264
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        let size = self.capacity() * (size_of::<T>() + size_of::<usize>());

        if T::IS_DYNAMIC {
            self.iter()
                .fold(size, |n, value| n + value.estimate_heap_size())
        } else {
            size + self.capacity() * T::STATIC_HEAP_SIZE
        }
    }
}

#[cfg(test)]
mod tests {
    use crate as datasize; // Required for the derive macro.
    use crate::{data_size, DataSize};

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
    fn test_string() {
        let value = "abcdef".to_string();

        assert_eq!(data_size(&value), 6);
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
    fn test_generic_newtype_struct() {
        #[derive(DataSize)]
        struct Foo<T>(T);

        assert!(!Foo::<Box<u32>>::IS_DYNAMIC);
        assert_eq!(Foo::<Box<u32>>::STATIC_HEAP_SIZE, 4);
        assert_eq!(data_size(&Foo(Box::new(123u32))), 4);
    }

    #[test]
    fn test_generic_tuple_struct() {
        #[derive(DataSize)]
        struct Foo<T>(T, Box<u8>, #[data_size(skip)] Box<u32>);

        assert!(!Foo::<Box<u32>>::IS_DYNAMIC);
        assert_eq!(Foo::<Box<u32>>::STATIC_HEAP_SIZE, 5);
        assert_eq!(
            data_size(&Foo(Box::new(123u32), Box::new(45), Box::new(0))),
            5
        );
    }
}
