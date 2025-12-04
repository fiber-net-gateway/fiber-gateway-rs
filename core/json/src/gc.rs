use std::cell::Cell;
use std::fmt;
use std::ops::Deref;
use std::ptr::NonNull;

/// Internal allocation with manual reference counts.
struct GcBox<T> {
    header: Counts,
    value: T,
}

struct Counts {
    root_count: Cell<usize>,
    non_root_count: Cell<usize>,
}

impl Counts {
    fn inc_root(&self) {
        self.root_count.set(
            self.root_count
                .get()
                .checked_add(1)
                .expect("root_count overflow"),
        );
    }

    fn dec_root(&self) {
        let current = self.root_count.get();
        debug_assert!(current > 0, "root_count underflow");
        self.root_count.set(current - 1);
    }

    fn inc_non_root(&self) {
        self.non_root_count.set(
            self.non_root_count
                .get()
                .checked_add(1)
                .expect("non_root_count overflow"),
        );
    }

    fn dec_non_root(&self) {
        let current = self.non_root_count.get();
        debug_assert!(current > 0, "non_root_count underflow");
        self.non_root_count.set(current - 1);
    }
}

/// Rooted reference that keeps a value alive.
pub struct GcRef<T> {
    ptr: NonNull<GcBox<T>>,
}

/// Non-root pointer used to model references between GC-managed values.
pub struct GcPointer<T> {
    ptr: NonNull<GcBox<T>>,
}

impl<T> GcRef<T> {
    /// Allocate a new GC-managed value with a single root.
    pub fn new(value: T) -> Self {
        let boxed = Box::new(GcBox {
            header: Counts {
                root_count: Cell::new(1),
                non_root_count: Cell::new(0),
            },
            value,
        });
        // SAFETY: Box never yields a null pointer.
        Self {
            ptr: unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) },
        }
    }

    /// Create a non-root pointer to this allocation.
    pub fn downgrade(&self) -> GcPointer<T> {
        let header = unsafe { &self.ptr.as_ref().header };
        header.inc_non_root();
        GcPointer { ptr: self.ptr }
    }

    /// Consume this root and turn it into a non-root pointer.
    pub fn into_pointer(self) -> GcPointer<T> {
        let pointer = self.downgrade();
        // Dropping `self` now will decrease the root_count, which is what we want.
        std::mem::drop(self);
        pointer
    }

    /// Current root reference count.
    pub fn root_count(&self) -> usize {
        unsafe { self.ptr.as_ref().header.root_count.get() }
    }

    /// Current non-root reference count.
    pub fn non_root_count(&self) -> usize {
        unsafe { self.ptr.as_ref().header.non_root_count.get() }
    }

    fn try_cleanup(&self) {
        // SAFETY: self.ptr is valid for the lifetime of the allocation.
        unsafe { try_cleanup(self.ptr) }
    }
}

impl<T> Clone for GcRef<T> {
    fn clone(&self) -> Self {
        let header = unsafe { &self.ptr.as_ref().header };
        header.inc_root();
        Self { ptr: self.ptr }
    }
}

impl<T> Deref for GcRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.ptr.as_ref().value }
    }
}

impl<T: fmt::Debug> fmt::Debug for GcRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("GcRef").field(&**self).finish()
    }
}

impl<T: PartialEq> PartialEq for GcRef<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Eq> Eq for GcRef<T> {}

impl<T> Drop for GcRef<T> {
    fn drop(&mut self) {
        let header = unsafe { &self.ptr.as_ref().header };
        header.dec_root();
        self.try_cleanup();
    }
}

impl<T> Clone for GcPointer<T> {
    fn clone(&self) -> Self {
        let header = unsafe { &self.ptr.as_ref().header };
        header.inc_non_root();
        Self { ptr: self.ptr }
    }
}

impl<T> GcPointer<T> {
    /// Upgrade this pointer into a rooted reference.
    pub fn upgrade(&self) -> GcRef<T> {
        let header = unsafe { &self.ptr.as_ref().header };
        header.inc_root();
        GcRef { ptr: self.ptr }
    }

    pub fn root_count(&self) -> usize {
        unsafe { self.ptr.as_ref().header.root_count.get() }
    }

    pub fn non_root_count(&self) -> usize {
        unsafe { self.ptr.as_ref().header.non_root_count.get() }
    }

    fn try_cleanup(&self) {
        // SAFETY: self.ptr is valid for the lifetime of the allocation.
        unsafe { try_cleanup(self.ptr) }
    }
}

impl<T> Deref for GcPointer<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &self.ptr.as_ref().value }
    }
}

impl<T> fmt::Debug for GcPointer<T>
where
    T: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_tuple("GcPointer").field(&**self).finish()
    }
}

impl<T: PartialEq> PartialEq for GcPointer<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Eq> Eq for GcPointer<T> {}

impl<T> Drop for GcPointer<T> {
    fn drop(&mut self) {
        let header = unsafe { &self.ptr.as_ref().header };
        header.dec_non_root();
        self.try_cleanup();
    }
}

unsafe fn try_cleanup<T>(ptr: NonNull<GcBox<T>>) {
    // SAFETY: caller guarantees `ptr` still points to a live allocation.
    unsafe {
        let header = &ptr.as_ref().header;
        if header.root_count.get() == 0 && header.non_root_count.get() == 0 {
            drop(Box::from_raw(ptr.as_ptr()));
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::cell::Cell;

    #[derive(PartialEq, Debug)]
    struct DropFlag<'a>(&'a Cell<bool>);

    impl Drop for DropFlag<'_> {
        fn drop(&mut self) {
            self.0.set(true);
        }
    }

    #[test]
    fn reference_counts_balance_between_roots_and_pointers() {
        let dropped = Cell::new(false);
        let root = GcRef::new(DropFlag(&dropped));
        assert_eq!(root.root_count(), 1);
        assert_eq!(root.non_root_count(), 0);

        let pointer = root.downgrade();
        assert_eq!(pointer.root_count(), 1);
        assert_eq!(pointer.non_root_count(), 1);

        {
            let pointer_clone = pointer.clone();
            assert_eq!(pointer_clone.non_root_count(), 2);
            drop(pointer_clone);
            assert_eq!(pointer.non_root_count(), 1);
        }

        drop(root);
        assert_eq!(pointer.root_count(), 0);
        assert!(!dropped.get());

        drop(pointer);
        assert!(dropped.get());
    }

    #[test]
    fn upgrade_creates_new_root() {
        let root = GcRef::new(42);
        let pointer = root.downgrade();
        drop(root);
        assert_eq!(pointer.root_count(), 0);

        let upgraded = pointer.upgrade();
        assert_eq!(upgraded.root_count(), 1);
        assert_eq!(*upgraded, 42);
    }

    #[test]
    fn drops_cycles_when_roots_disappear() {
        use std::cell::RefCell;
        use std::rc::Rc;

        #[derive(Debug)]
        struct Node {
            flag: Rc<Cell<usize>>,
            next: RefCell<Option<GcPointer<Node>>>,
        }

        impl Drop for Node {
            fn drop(&mut self) {
                self.flag.set(self.flag.get() + 1);
            }
        }

        let dropped = Rc::new(Cell::new(0));
        let node_a = GcRef::new(Node {
            flag: dropped.clone(),
            next: RefCell::new(None),
        });
        let node_b = GcRef::new(Node {
            flag: dropped.clone(),
            next: RefCell::new(None),
        });

        node_a.next.borrow_mut().replace(node_b.downgrade());
        node_b.next.borrow_mut().replace(node_a.downgrade());

        assert_eq!(dropped.get(), 0);

        // Break the cycle before releasing the final roots so drops can cascade.
        node_a.next.borrow_mut().take();
        node_b.next.borrow_mut().take();

        drop(node_a);
        drop(node_b);

        assert_eq!(dropped.get(), 2);
    }
}
