//! Pointer interning for flat elaborated-design JSON export.
//!
//! Each export kind (`type`, `value`, `memory`, `nbr_sources`, …) has its own
//! contiguous 1-based ID space.
//!
//! AI NOTICE: Mostly generated, partially reviewed.

use std::ptr::NonNull;

use rustc_hash::FxHashMap;

/// Number of [`ExportObjectKind`] variants.
const KIND_COUNT: usize = 5;

/// Object kinds for pointer-backed elaboration data.
#[repr(u32)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ExportObjectKind {
    Type = 0,
    Value = 1,
    Memory = 2,
    NbrSources = 3,
    RecElArray = 4,
}

impl TryFrom<u32> for ExportObjectKind {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::Type),
            1 => Ok(Self::Value),
            2 => Ok(Self::Memory),
            3 => Ok(Self::NbrSources),
            4 => Ok(Self::RecElArray),
            _ => Err(()),
        }
    }
}

struct InternedEntry {
    ptr: NonNull<u8>,
    size: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct InternedId(u32);

/// Interning context for elaborated-design export.
pub struct DesignExportContext {
    /// Maps from *(kind, address)* to *per-kind interned ID*.
    map: FxHashMap<(ExportObjectKind, NonNull<u8>), InternedId>,
    /// Interned entries per kind, indexed by that kind's 1-based ID.
    entries: [Vec<InternedEntry>; KIND_COUNT],
}

impl DesignExportContext {
    fn new() -> Self {
        Self {
            map: FxHashMap::default(),
            entries: Default::default(),
        }
    }

    fn intern(
        &mut self,
        kind: ExportObjectKind,
        ptr: NonNull<u8>,
        size: u32,
    ) -> (InternedId, bool) {
        if let Some(&id) = self.map.get(&(kind, ptr)) {
            return (id, false);
        }

        let table = &mut self.entries[kind as usize];
        let id = InternedId(u32::try_from(table.len() + 1).expect("export id overflow"));
        self.map.insert((kind, ptr), id);
        table.push(InternedEntry {
            ptr,
            size: size as usize,
        });
        (id, true)
    }

    fn count(&self, kind: ExportObjectKind) -> u32 {
        u32::try_from(self.entries[kind as usize].len()).unwrap_or(0)
    }

    fn entry(&self, kind: ExportObjectKind, id: InternedId) -> Option<&InternedEntry> {
        if id.0 == 0 {
            return None;
        }
        self.entries[kind as usize].get(usize::try_from(id.0 - 1).ok()?)
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_create() -> Box<DesignExportContext> {
    Box::new(DesignExportContext::new())
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_free(ctx: Box<DesignExportContext>) {
    drop(ctx);
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_intern(
    ctx: &mut DesignExportContext,
    kind: u32,
    ptr: NonNull<u8>,
    size: u32,
    id: &mut InternedId,
    was_new: &mut bool,
) {
    let Ok(kind) = ExportObjectKind::try_from(kind) else {
        *id = InternedId(0);
        *was_new = false;
        return;
    };
    (*id, *was_new) = ctx.intern(kind, ptr, size);
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_count(ctx: &DesignExportContext, kind: u32) -> u32 {
    let Ok(kind) = ExportObjectKind::try_from(kind) else {
        return 0;
    };
    ctx.count(kind)
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_get_size(
    ctx: &DesignExportContext,
    kind: u32,
    id: InternedId,
) -> u32 {
    let Ok(kind) = ExportObjectKind::try_from(kind) else {
        return 0;
    };
    ctx.entry(kind, id)
        .map_or(0, |entry| u32::try_from(entry.size).unwrap_or(0))
}

/// Returns the interned entry for `kind` with per-kind ID `id`.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_get_entry(
    ctx: &DesignExportContext,
    kind: u32,
    id: InternedId,
    ptr: &mut *const u8,
    size: &mut u32,
) {
    let Ok(kind) = ExportObjectKind::try_from(kind) else {
        *ptr = std::ptr::null();
        *size = 0;
        return;
    };
    if let Some(entry) = ctx.entry(kind, id) {
        *ptr = entry.ptr.as_ptr();
        *size = u32::try_from(entry.size).unwrap_or(0);
    } else {
        *ptr = std::ptr::null();
        *size = 0;
    }
}

/// Appends the interned entry as a JSON string of Base64-encoded bytes.
#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_append_memory_base64(
    buffer: &mut Vec<u8>,
    ctx: &DesignExportContext,
    kind: u32,
    id: InternedId,
) {
    let Ok(kind) = ExportObjectKind::try_from(kind) else {
        return;
    };
    let Some(entry) = ctx.entry(kind, id) else {
        return;
    };

    buffer.push(b'"');
    // SAFETY: `entry.ptr`/`entry.size` come from the elaborator and stay valid
    // for the lifetime of the export context.
    let bytes = unsafe { std::slice::from_raw_parts(entry.ptr.as_ptr(), entry.size) };
    append_base64(buffer, bytes);
    buffer.push(b'"');
}

/// Appends standard Base64 encoding of `bytes` to `buffer`.
fn append_base64(buffer: &mut Vec<u8>, bytes: &[u8]) {
    const TABLE: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut chunks = bytes.chunks_exact(3);
    for chunk in chunks.by_ref() {
        let n = (u32::from(chunk[0]) << 16) | (u32::from(chunk[1]) << 8) | u32::from(chunk[2]);
        buffer.push(TABLE[((n >> 18) & 0x3f) as usize]);
        buffer.push(TABLE[((n >> 12) & 0x3f) as usize]);
        buffer.push(TABLE[((n >> 6) & 0x3f) as usize]);
        buffer.push(TABLE[(n & 0x3f) as usize]);
    }
    let rem = chunks.remainder();
    match rem.len() {
        1 => {
            let n = u32::from(rem[0]) << 16;
            buffer.push(TABLE[((n >> 18) & 0x3f) as usize]);
            buffer.push(TABLE[((n >> 12) & 0x3f) as usize]);
            buffer.push(b'=');
            buffer.push(b'=');
        },
        2 => {
            let n = (u32::from(rem[0]) << 16) | (u32::from(rem[1]) << 8);
            buffer.push(TABLE[((n >> 18) & 0x3f) as usize]);
            buffer.push(TABLE[((n >> 12) & 0x3f) as usize]);
            buffer.push(TABLE[((n >> 6) & 0x3f) as usize]);
            buffer.push(b'=');
        },
        _ => {},
    }
}

#[cfg(test)]
mod tests {
    use super::append_base64;

    fn encode_base64(bytes: &[u8]) -> String {
        let mut buffer = Vec::new();
        append_base64(&mut buffer, bytes);
        String::from_utf8(buffer).unwrap()
    }

    /// RFC 4648 §10 test vectors.
    const RFC4648_VECTORS: &[(&[u8], &str)] = &[
        (b"", ""),
        (b"f", "Zg=="),
        (b"fo", "Zm8="),
        (b"foo", "Zm9v"),
        (b"foob", "Zm9vYg=="),
        (b"fooba", "Zm9vYmE="),
        (b"foobar", "Zm9vYmFy"),
    ];

    #[test]
    fn encode_rfc4648_vectors() {
        for &(plain, encoded) in RFC4648_VECTORS {
            assert_eq!(encode_base64(plain), encoded, "encode {plain:?}");
        }
    }

    #[test]
    fn encode_all_bytes() {
        let bytes: Vec<u8> = (0u8..=255).collect();
        let encoded = encode_base64(&bytes);
        assert!(encoded.is_ascii());
        assert_eq!(encoded.len() % 4, 0);
        // 256 ≡ 1 (mod 3), so encoding ends with `==`.
        assert!(encoded.ends_with("=="));
    }
}
