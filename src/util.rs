

static mut ID: u32 = 0;

/* Allows assigning a unique id to various entities. Not safe for multithreaded
 * code, literally a race condition. */
pub fn next_id() -> u32 {
    unsafe {
        let ret_val = ID;
        ID += 1;
        ret_val
    }
}
