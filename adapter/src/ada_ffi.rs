use std::marker::PhantomData;
use std::ops;

#[derive(Copy, Clone, Debug)]
#[repr(C)]
pub struct AdaString<'a> {
    ptr: *const u8,
    len: u64,
    _marker: PhantomData<&'a u8>,
}

impl<'a> AdaString<'a> {
    /// Returns the string bytes.
    pub fn chars(&self) -> &'a [u8] {
        if self.len != 0 && !self.ptr.is_null() {
            unsafe { std::slice::from_raw_parts(self.ptr, self.len as usize) }
        } else {
            &[]
        }
    }
}

impl ops::Deref for AdaString<'_> {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        self.chars()
    }
}

impl From<AdaString<'_>> for String {
    fn from(value: AdaString<'_>) -> Self {
        latin1_bytes_to_string(&value)
    }
}

/// Decodes a VHDL Latin-1 string into a Rust `String`.
fn latin1_bytes_to_string(bytes: &[u8]) -> String {
    let mut string = String::with_capacity(bytes.len());
    string.extend(bytes.iter().map(|&byte| char::from(byte)));
    string
}

/// Decodes a null-terminated Latin-1 C string into a Rust `String`.
///
/// # Safety
///
/// - The memory pointed to by `ptr` must contain a valid nul terminator at the end of the string.
/// - `ptr` must be [valid] for reads of bytes up to and including the nul terminator.
/// - The nul terminator must be within `isize::MAX` from `ptr`
pub unsafe fn latin1_c_string_to_string(ptr: *const u8) -> String {
    if ptr.is_null() {
        String::new()
    } else {
        // SAFETY: Valid as per function preconditions, plus the memory won't be mutated.
        let string = unsafe { std::ffi::CStr::from_ptr(ptr.cast()) };
        latin1_bytes_to_string(string.to_bytes())
    }
}
