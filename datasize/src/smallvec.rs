use super::DataSize;
use core::mem::size_of;

impl<A> DataSize for smallvec::SmallVec<A>
where
    A: smallvec::Array,
    A::Item: DataSize,
{
    const IS_DYNAMIC: bool = true;

    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        if !self.spilled() {
            return 0;
        }

        // At this point, we're very similar to a regular `Vec`.

        let sz_base = self.capacity() * size_of::<A::Item>();

        let sz_used = if A::Item::IS_DYNAMIC {
            self.iter().map(DataSize::estimate_heap_size).sum()
        } else {
            self.len() * A::Item::STATIC_HEAP_SIZE
        };

        sz_base + sz_used
    }
}
