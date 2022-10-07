use super::{data_size, non_dynamic_const_heap_size, DataSize};

use core::mem::size_of;

use std::{borrow::Cow, boxed::Box, string::String, vec::Vec};

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

impl<'a, T> DataSize for Cow<'a, T>
where
    T: 'a + ToOwned + ?Sized,
    <T as ToOwned>::Owned: DataSize,
{
    const IS_DYNAMIC: bool = true;
    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        match self {
            Cow::Borrowed(_) => 0,
            Cow::Owned(inner) => inner.estimate_heap_size(),
        }
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

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        self.capacity()
    }
}

impl DataSize for std::ffi::OsString {
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
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

    #[inline]
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

impl<T> DataSize for std::collections::BTreeSet<T>
where
    T: DataSize,
{
    // A BTreeSet<T> is implemented as BTreeMap<T, ()> in the standard library, so we use the same
    // estimate as above.

    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        if T::IS_DYNAMIC {
            self.len() * size_of::<T>() + self.iter().map(T::estimate_heap_size).sum::<usize>()
        } else {
            self.len() * (size_of::<T>() + T::STATIC_HEAP_SIZE)
        }
    }
}

fn estimate_hashbrown_rawtable<T>(capacity: usize) -> usize {
    // https://github.com/rust-lang/hashbrown/blob/v0.12.3/src/raw/mod.rs#L185
    let buckets = if capacity < 8 {
        if capacity < 4 {
            4
        } else {
            8
        }
    } else {
        (capacity * 8 / 7).next_power_of_two()
    };
    // https://github.com/rust-lang/hashbrown/blob/v0.12.3/src/raw/mod.rs#L242
    let size = size_of::<T>();
    // `Group` is u32, u64, or __m128i depending on the CPU architecture.
    // Return a lower bound, ignoring its constant contributions
    // (through ctrl_align and Group::WIDTH, at most 31 bytes).
    let ctrl_offset = size * buckets;
    ctrl_offset + buckets
}

impl<K, V, S> DataSize for std::collections::HashMap<K, V, S>
where
    K: DataSize,
    V: DataSize,
{
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        let size = estimate_hashbrown_rawtable::<(K, V)>(self.capacity());

        if K::IS_DYNAMIC || V::IS_DYNAMIC {
            size + self
                .iter()
                .map(|(k, v)| k.estimate_heap_size() + v.estimate_heap_size())
                .sum::<usize>()
        } else {
            size + self.len() * (K::STATIC_HEAP_SIZE + V::STATIC_HEAP_SIZE)
        }
    }
}

impl<T, S> DataSize for std::collections::HashSet<T, S>
where
    T: DataSize,
{
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        // HashSet<T> is based on HashMap<T, ()>
        let size = estimate_hashbrown_rawtable::<(T, ())>(self.capacity());

        if T::IS_DYNAMIC {
            size + self.iter().map(T::estimate_heap_size).sum::<usize>()
        } else {
            size + self.len() * T::STATIC_HEAP_SIZE
        }
    }
}

#[cfg(test)]
mod tests {
    use crate as datasize; // Required for the derive macro.
    use crate::{data_size, DataSize};
    use std::borrow::Cow;

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
    fn test_cow() {
        let value: Cow<'static, str> = Cow::from("hello");
        assert_eq!(data_size(&value), 0);

        let value_owned: Cow<'static, str> = Cow::from("hello".to_owned());

        assert_eq!(data_size(&value_owned), data_size(&"hello".to_owned()));
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
    fn test_data_size_inner_box() {
        #[derive(Clone, DataSize)]
        struct Inner {
            value: Box<u64>, // sz: ptr, heap: 8
            dummy: u8,       // sz: ptr
        }
        // total: sz 16, heap 8

        let inner = Inner {
            value: Box::new(0), // sz ptr, heap: 2*ptr+8
            dummy: 0,           // sz ptr
        };
        // total: 24

        let boxed = Box::new(inner.clone());

        let inner_size = core::mem::size_of::<Inner>();
        assert_eq!(8, data_size(&inner));
        assert_eq!(8 + inner_size, data_size(&boxed));
    }

    #[test]
    #[cfg(feature = "detailed")]
    fn test_nested_detailed_struct() {
        #[derive(DataSize)]
        struct Inner {
            value: Box<u64>,
            dummy: u8,
        }

        #[derive(DataSize)]
        struct Outer {
            a: Box<Inner>,
            b: Inner,
            c: u8,
        }

        let fixture = Outer {
            a: Box::new(Inner {
                value: Box::new(1),
                dummy: 42,
            }),
            b: Inner {
                value: Box::new(2),
                dummy: 42,
            },
            c: 3,
        };

        let detailed = datasize::data_size_detailed(&fixture);

        use datasize::MemUsageNode;
        use std::collections::HashMap;

        let mut inner_map = HashMap::new();
        inner_map.insert("value", MemUsageNode::Size(8));
        inner_map.insert("dummy", MemUsageNode::Size(0));

        let mut outer_map = HashMap::new();
        outer_map.insert("a", MemUsageNode::Size(24));
        outer_map.insert("b", MemUsageNode::Detailed(inner_map));
        outer_map.insert("c", MemUsageNode::Size(0));

        let expected = MemUsageNode::Detailed(outer_map);

        assert_eq!(detailed, expected);

        assert_eq!(data_size(&fixture), detailed.total());
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
