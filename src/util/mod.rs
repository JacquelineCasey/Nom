static mut ID: u32 = 0;

/// Allows assigning a unique id to various entities. Not safe for multithreaded
/// code, literally a race condition.
pub fn next_id() -> u32 {
    unsafe {
        assert!(ID != u32::MAX, "Ran out of unique ids!");

        let ret_val = ID;
        ID += 1;
        ret_val
    }
}

/// Refer to: <https://users.rust-lang.org/t/transmuting-a-generic-array/45645/5>
/// Types must be same size, and this is not checked.
pub fn reinterpret<In: Copy, Out: Copy>(i: In) -> Out {
    let ptr = std::ptr::addr_of!(i).cast::<Out>();
    unsafe { *ptr }
}

/// An out stream allows us to push values to it, but not pull. Basically a safer way of passing around a mut& Vec while
/// maintaining that we only ever push to it.
pub struct OutStream<'a, T> {
    buffer: &'a mut Vec<T>,
}

impl<'a, T> OutStream<'a, T> {
    pub fn new(buffer: &mut Vec<T>) -> OutStream<'_, T> {
        OutStream { buffer }
    }

    pub fn push(&mut self, t: T) {
        self.buffer.push(t);
    }
}

/// A simple type wrapping either a file input, or a direct string input.
#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum FileOrString {
    /// Represents input via a file, given by a string.
    File(String),
    /// Represents direct string input. The first string is a "Fake Path" for use
    /// in diagnostics, such as "\<input\>". The second string is the full program.
    String(String, std::rc::Rc<String>),
}
