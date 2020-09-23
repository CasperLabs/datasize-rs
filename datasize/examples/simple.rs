use datasize::{data_size, DataSize};

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

fn main() {
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
