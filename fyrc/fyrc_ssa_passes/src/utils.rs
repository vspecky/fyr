use std::{
    any::{Any, TypeId},
    cell::{Ref, RefCell, RefMut},
};

pub(crate) trait RefCellExt {
    fn downcast<T: Any>(&self) -> Option<Ref<T>>;
    fn downcast_mut<T: Any>(&self) -> Option<RefMut<T>>;
}

impl RefCellExt for RefCell<Box<dyn Any>> {
    fn downcast<T: Any>(&self) -> Option<Ref<T>> {
        let borrowed = self.borrow();
        if (**borrowed).type_id() == TypeId::of::<T>() {
            // Panic Safety: We have already checked that the type id of the type
            // contained within the RefCell<dyn Any> is the same as the one of the type
            // we want
            Some(Ref::map(borrowed, |t| t.downcast_ref().unwrap()))
        } else {
            None
        }
    }

    fn downcast_mut<T: Any>(&self) -> Option<RefMut<T>> {
        let borrowed = self.borrow_mut();
        if (**borrowed).type_id() == TypeId::of::<T>() {
            // Panic Safety: We have already checked that the type id of the type
            // contained within the RefCell<dyn Any> is the same as the one of the type
            // we want
            Some(RefMut::map(borrowed, |t| t.downcast_mut().unwrap()))
        } else {
            None
        }
    }
}
