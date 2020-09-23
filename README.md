# data_size::&lt;T&gt;(&T)

The `datasize` crate is used for for estimating the heap memory usage of values, e.g. the number of bytes used by a `Vec` outside its on-stack size determined by `mem::size_of`.

`datasize` is intended for rough benchmarks, typically to find memory hogs (it won't find memory leaks as, as memory that is not reachable will not be reported). While it may not give entirely accurate readings in all situations, it will quickly identify low-hanging fruits.

## Example

The `DataSize` trait is implemented for many primitive and `std` types, and can be derived for `structs` and others:

```rust
use datasize::DataSize;

#[derive(DataSize)]
struct Example {
    count: usize,
    my_data: Vec<MyStruct>,
    warning: Option<Box<u32>>,
    #[data_size(skip)]
    skipped: Box<char>,
}
```

Any instance `ex` of the `Example` can now report an estimate of its heap allocation through `data_size(&ex)`.

## Other works

Other crates suitable for this task are [`heapsize`](https://docs.rs/heapsize/) and [`malloc_size_of`](https://github.com/servo/servo/tree/faf3a183f3755a9986ec4379abadf3523bd8b3c0/components/malloc_size_of). Unfortunately the `heapsize` crate has been discontinued and the `malloc_size_of` crate puts some rather heavy constraints on what allocator can be used.
