use std::io::Write;

use crate::ada_ffi::AdaString;

#[expect(
    clippy::box_collection,
    reason = "we want a pointer to a Vec<u8> for FFI"
)]
#[unsafe(no_mangle)]
pub extern "C" fn adapter_create_buffer(capacity: u32) -> Box<Vec<u8>> {
    Box::new(Vec::with_capacity(capacity as usize))
}

#[expect(
    clippy::box_collection,
    reason = "we want a pointer to a Vec<u8> for FFI"
)]
#[unsafe(no_mangle)]
pub extern "C" fn adapter_free_buffer(buffer: Box<Vec<u8>>) {
    drop(buffer);
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_char(buffer: &mut Vec<u8>, c: u8) {
    buffer.push(c);
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_str(buffer: &mut Vec<u8>, str: AdaString<'_>) {
    buffer.extend_from_slice(&str);
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_escaped(buffer: &mut Vec<u8>, str: AdaString<'_>) {
    append_escaped(buffer, str);
}

fn append_escaped(buffer: &mut Vec<u8>, str: AdaString<'_>) {
    for &byte in &*str {
        match byte {
            b'\\' | b'"' => {
                buffer.push(b'\\');
                buffer.push(byte);
            },

            0..=31 => {
                let hex_chars = [
                    b'0', b'1', b'2', b'3', b'4', b'5', b'6', b'7', b'8', b'9', b'a', b'b', b'c',
                    b'd', b'e', b'f',
                ];
                let mut bytes = [b'\\', b'u', b'0', b'0', 0, 0];
                bytes[4] = hex_chars[byte as usize / 16];
                bytes[5] = hex_chars[byte as usize % 16];
                buffer.extend_from_slice(&bytes);
            },

            128..=255 => {
                buffer.push(0xc0 + (byte >> 6));
                buffer.push(0x80 + (byte & 0x3f));
            },

            _ => {
                buffer.push(byte);
            },
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_bool(buffer: &mut Vec<u8>, value: bool) {
    let _wont_fail = write!(buffer, "{}", if value { "true" } else { "false" });
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_u32(buffer: &mut Vec<u8>, value: u32) {
    let _wont_fail = write!(buffer, "{value}");
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_i32(buffer: &mut Vec<u8>, value: i32) {
    let _wont_fail = write!(buffer, "{value}");
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_i64(buffer: &mut Vec<u8>, value: i64) {
    let _wont_fail = write!(buffer, "{value}");
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_f64(buffer: &mut Vec<u8>, value: f64) {
    let _wont_fail = write!(buffer, "\"#{:x}\"", value.to_bits());
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_ptr(buffer: &mut Vec<u8>, value: *const u8) {
    let _wont_fail = write!(buffer, "\"{value:p}\"");
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_bool_attribute(
    buffer: &mut Vec<u8>,
    attr: AdaString<'_>,
    value: bool,
) {
    start_attribute(buffer, attr);
    let _wont_fail = write!(buffer, "\":{}", if value { "true" } else { "false" });
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_u32_attribute(
    buffer: &mut Vec<u8>,
    attr: AdaString<'_>,
    value: u32,
) {
    start_attribute(buffer, attr);
    let _wont_fail = write!(buffer, "\":{value}");
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_i32_attribute(
    buffer: &mut Vec<u8>,
    attr: AdaString<'_>,
    value: i32,
) {
    start_attribute(buffer, attr);
    let _wont_fail = write!(buffer, "\":{value}");
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_i64_attribute(
    buffer: &mut Vec<u8>,
    attr: AdaString<'_>,
    value: i64,
) {
    start_attribute(buffer, attr);
    let _wont_fail = write!(buffer, "\":{value}");
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_f64_attribute(
    buffer: &mut Vec<u8>,
    attr: AdaString<'_>,
    value: f64,
) {
    start_attribute(buffer, attr);
    let _wont_fail = write!(buffer, "\":\"#{:x}\"", value.to_bits());
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_append_string_attribute(
    buffer: &mut Vec<u8>,
    attr: AdaString<'_>,
    value: AdaString<'_>,
) {
    start_attribute(buffer, attr);
    buffer.extend_from_slice(b"\":\"");
    append_escaped(buffer, value);
    buffer.push(b'"');
}

fn start_attribute(buffer: &mut Vec<u8>, attr: AdaString<'_>) {
    buffer.extend_from_slice(b",\"");
    buffer.extend_from_slice(&attr);
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_flush(buffer: &mut Vec<u8>) {
    let mut stdout = std::io::stdout().lock();
    if let Err(e) = stdout.write_all(buffer) {
        eprintln!("Error writing to stdout: {e}");
        // We mustn't panic in an FFI function, so we exit the whole program
        std::process::exit(2);
    }
    let _ignored = stdout.flush();
    buffer.clear();
}
