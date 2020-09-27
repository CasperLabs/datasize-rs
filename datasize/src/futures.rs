use super::DataSize;
use core::mem::size_of;

impl<T> DataSize for futures::channel::oneshot::Sender<T> {
    const IS_DYNAMIC: bool = false;
    const STATIC_HEAP_SIZE: usize = 0;

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        0
    }
}

// Note: We attribute the value to be sent/received to the receiver. This is still wildly inaccurate
// though.

impl<T> DataSize for futures::channel::oneshot::Receiver<T> {
    const IS_DYNAMIC: bool = false;
    const STATIC_HEAP_SIZE: usize = size_of::<T>() + 3 * size_of::<usize>();

    #[inline]
    fn estimate_heap_size(&self) -> usize {
        // Missing: Lock<Option<Waker>>, approximated by three pointers each.
        Self::STATIC_HEAP_SIZE
    }
}
