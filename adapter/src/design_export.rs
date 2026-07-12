//! Pointer interning for flat elaborated-design JSON export.
//!
//! AI NOTICE: Mostly generated, partially reviewed.

use std::io::Write;
use std::ptr::NonNull;

use rustc_hash::FxHashMap;

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
    kind: ExportObjectKind,
    ptr: NonNull<u8>,
    size: usize,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[repr(transparent)]
pub struct InternedId(u32);

/// Interning context for elaborated-design export.
pub struct DesignExportContext {
    /// Maps from *(kind, address)* to *interned ID*.
    map: FxHashMap<(ExportObjectKind, NonNull<u8>), InternedId>,
    /// Interned entries, indexed by *interned ID*.
    entries: Vec<InternedEntry>,
    /// Object counts, indexed by [`ExportObjectKind`].
    counts: [u32; 5],
}

impl DesignExportContext {
    fn new() -> Self {
        Self {
            map: FxHashMap::default(),
            entries: Vec::new(),
            counts: [0; 5],
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

        let id = InternedId(u32::try_from(self.entries.len() + 1).expect("export id overflow"));
        self.map.insert((kind, ptr), id);
        self.entries.push(InternedEntry {
            kind,
            ptr,
            size: size as usize,
        });
        self.counts[kind as usize] += 1;
        (id, true)
    }

    fn count(&self, kind: ExportObjectKind) -> u32 {
        self.counts[kind as usize]
    }

    fn entry(&self, kind: ExportObjectKind, id: InternedId) -> Option<&InternedEntry> {
        if id.0 == 0 {
            return None;
        }
        let entry = self.entries.get(usize::try_from(id.0 - 1).ok()?)?;
        if entry.kind == kind {
            Some(entry)
        } else {
            None
        }
    }

    fn entry_at(&self, index: InternedId) -> Option<&InternedEntry> {
        if index.0 == 0 {
            return None;
        }
        self.entries.get(usize::try_from(index.0 - 1).ok()?)
    }

    fn total_entries(&self) -> u32 {
        u32::try_from(self.entries.len()).unwrap_or(0)
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

#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_get_entry(
    ctx: &DesignExportContext,
    index: InternedId,
    kind: &mut u32,
    ptr: &mut *const u8,
    size: &mut u32,
) {
    if let Some(entry) = ctx.entry_at(index) {
        *kind = entry.kind as u32;
        *ptr = entry.ptr.as_ptr();
        *size = u32::try_from(entry.size).unwrap_or(0);
    } else {
        *kind = 0;
        *ptr = std::ptr::null();
        *size = 0;
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn adapter_design_export_total_entries(ctx: &DesignExportContext) -> u32 {
    ctx.total_entries()
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
