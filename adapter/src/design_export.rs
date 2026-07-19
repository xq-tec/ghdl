//! Pointer interning for flat elaborated-design JSON export.
//!
//! Each export kind (`type`, `value`, `memory`, `nbr_sources`, …) has its own
//! contiguous 1-based ID space.
//!
//! AI NOTICE: Mostly generated, partially reviewed.

use std::io::Write;
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

#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_append_memory_hex(
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
    let bytes = unsafe { std::slice::from_raw_parts(entry.ptr.as_ptr(), entry.size) };
    const HEX: &[u8; 16] = b"0123456789abcdef";
    for byte in bytes {
        let _wont_fail = write!(
            buffer,
            "{}{}",
            HEX[(byte >> 4) as usize],
            HEX[(byte & 0xf) as usize]
        );
    }
    buffer.push(b'"');
}
