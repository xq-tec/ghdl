#![expect(clippy::missing_safety_doc, reason = "WIP")]

use std::io::Write;

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_create_buffer(size: u32) -> *mut Vec<u8> {
    Box::into_raw(Box::new(Vec::with_capacity(size as usize)))
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_free_buffer(buffer: *mut Vec<u8>) {
    drop(unsafe { Box::from_raw(buffer) });
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_append_char(buffer: &mut Vec<u8>, c: u8) {
    buffer.push(c);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_append_str(buffer: &mut Vec<u8>, str: *const u8, len: u64) {
    let byte_slice = unsafe { std::slice::from_raw_parts(str, len as usize) };
    buffer.extend_from_slice(byte_slice);
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_append_escaped(buffer: &mut Vec<u8>, str: *const u8, len: u64) {
    let byte_slice = unsafe { std::slice::from_raw_parts(str, len as usize) };
    for &byte in byte_slice {
        match byte {
            b'\\' | b'"' => {
                buffer.push(b'\\');
                buffer.push(byte);
            }

            0..=31 => {
                let hex_chars = [
                    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c',
                    b'd', b'e', b'f',
                ];
                let mut bytes = [b'\\', b'u', b'0', b'0', 0, 0];
                bytes[4] = hex_chars[byte as usize / 16];
                bytes[5] = hex_chars[byte as usize % 16];
                buffer.extend_from_slice(&bytes);
            }

            128..=255 => {
                buffer.push(0xc0 + (byte >> 6));
                buffer.push(0x80 + (byte & 0x3f));
            }

            _ => {
                buffer.push(byte);
            }
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_append_u32(buffer: &mut Vec<u8>, value: u32) {
    write!(buffer, "{value}").unwrap();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_append_i32(buffer: &mut Vec<u8>, value: i32) {
    write!(buffer, "{value}").unwrap();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_append_i64(buffer: &mut Vec<u8>, value: i64) {
    write!(buffer, "{value}").unwrap();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_append_f64(buffer: &mut Vec<u8>, value: f64) {
    write!(buffer, "\"#{:x}\"", value.to_bits()).unwrap();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn adapter_flush(buffer: &mut Vec<u8>) {
    std::io::stdout().write_all(buffer).unwrap();
    buffer.clear();
}
