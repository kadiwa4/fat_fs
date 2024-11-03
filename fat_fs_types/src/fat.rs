//! FAT12/16/32.

use bitflags::bitflags;
#[cfg(feature = "bytemuck")]
use bytemuck::{Pod, Zeroable};
#[cfg(feature = "zerocopy")]
use zerocopy::{FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned};

use crate::{ClusterIdx, Lba, U16Le, U32Le};

/// Number of special sentinel values that indicate the end of an allocation.
pub const END_OF_ALLOC_MARK_COUNT: ClusterIdx = 8;
pub const FREE_CLUSTER_MARK: ClusterIdx = 0;
pub const MAX_CLUSTER_SIZE: usize = 0x8000;

/// Minimum and maximum FAT lengths for every FAT type.
///
/// **Note:** This is not the same as the cluster count; this is the number of
/// entries in a FAT and not the number of clusters on the heap.
/// The `*_MAX` values also never indicate the maximum valid cluster index for
/// file systems of that FAT type. They are one larger than the maximum valid
/// cluster index could ever be.
pub mod fat_len {
	use crate::ClusterIdx;

	pub const FAT12_MAX: ClusterIdx = 0x0FF6;
	pub const FAT16_MIN: ClusterIdx = 0x0FF7;
	pub const FAT16_MAX: ClusterIdx = 0xFFF6;
	pub const FAT32_MIN: ClusterIdx = 0xFFF7;
	pub const FAT32_MAX: ClusterIdx = 0x0FFF_FFF7;
}

bitflags! {
	/// Flags placed in the FAT entry at index 1.
	#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
	pub struct FatEntry1Flags: u8 {
		/// Set if no disk read/write errors were encountered.
		const NO_IO_ERROR = 4;
		/// Whether or not the file system driver dismounted the volume properly
		/// the last time it had the volume mounted.
		const CLEAN = 8;
	}
}

impl Default for FatEntry1Flags {
	#[inline]
	fn default() -> Self {
		Self::all()
	}
}

/// Maximum length of a long name.
pub const MAX_LONG_NAME_LEN: usize = 0xFF;
/// Maximum length of an 8.3 path.
pub const MAX_SHORT_PATH_LEN: usize = 0x4F;
/// Maximum length of a long name path.
pub const MAX_LONG_PATH_LEN: usize = 0x0103;
pub const NO_NAME: ShortName = *b"NO NAME    ";
pub const SELF_DIR: ShortName = *b".          ";
pub const PARENT_DIR: ShortName = *b"..         ";

/// An 8.3 name of a directory child.
pub type ShortName = [u8; 0xB];

/// Type of FAT file system: FAT12, FAT16 or FAT32.
///
/// For `Fat16` and `Fat32`, the discriminator is also the shift amount if you want
/// to convert between byte size and FAT entry count.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum FatType {
	Fat12,
	Fat16,
	Fat32,
}

impl FatType {
	/// Value to write to [`BootRecordTailExt::fat_type_name`][fat_type_name].
	///
	/// [fat_type_name]: boot_reg::BootRecordTailExt::fat_type_name
	#[inline]
	pub const fn name(self) -> [u8; 8] {
		match self {
			Self::Fat12 => *b"FAT12   ",
			Self::Fat16 => *b"FAT16   ",
			Self::Fat32 => *b"FAT32   ",
		}
	}

	#[inline]
	pub const fn fat_entry_bit_size(self) -> u8 {
		match self {
			Self::Fat12 => 0xC,
			Self::Fat16 => 0x10,
			Self::Fat32 => 0x20,
		}
	}

	#[inline]
	pub const fn default_fat_base_lba(self) -> Lba {
		match self {
			Self::Fat32 => 0x20,
			_ => 1,
		}
	}

	#[inline]
	pub const fn bad_cluster_mark(self) -> ClusterIdx {
		self.canon_end_of_alloc_mark() - END_OF_ALLOC_MARK_COUNT
	}

	/// Sentinel value indicating the end of an allocation.
	///
	/// There is more than one value reserved for this. This function will
	/// return the canonical value according to Microsoft.
	#[inline]
	pub const fn canon_end_of_alloc_mark(self) -> ClusterIdx {
		match self {
			Self::Fat12 => 0x0FFF,
			Self::Fat16 => 0xFFFF,
			Self::Fat32 => 0x0FFF_FFFF,
		}
	}

	#[inline]
	pub const fn fat_byte_size(self, entry_count: ClusterIdx) -> u32 {
		match self {
			Self::Fat12 => entry_count + (entry_count + 1) / 2,
			_ => entry_count * (1 << self as u8),
		}
	}

	pub const fn pack_fat_entry0(self, medium_type: u8) -> ClusterIdx {
		(match self {
			Self::Fat12 => 0x0F00,
			Self::Fat16 => 0xFF00,
			Self::Fat32 => 0x0FFF_FF00,
		}) | medium_type as u32
	}

	pub const fn pack_fat_entry1(self, flags: FatEntry1Flags) -> ClusterIdx {
		match self {
			Self::Fat12 => 0x0FFF,
			Self::Fat16 => 0x3FFF | (flags.bits() as u32) << 0xC,
			Self::Fat32 => 0x03FF_FFFF | (flags.bits() as u32) << 0x18,
		}
	}

	/// On FAT16/32, unpacks the flags placed in the FAT entry at index 1.
	#[inline]
	pub const fn unpack_fat_entry1(self, entry: ClusterIdx) -> Option<FatEntry1Flags> {
		if matches!(self, Self::Fat12) {
			None
		} else {
			Some(FatEntry1Flags::from_bits_retain(
				(entry >> (0xC * self as u8)) as u8,
			))
		}
	}
}

/// The "boot region" (aka "reserved region") of the FAT volume contains the
/// boot record and `FSInfo` structures.
///
/// Field names ending with `lba_size` indicate that the field counts the number
/// of blocks that something occupies (as opposed to byte size or bit size).
///
/// # First block
///
/// The beginning of the first block is structured like this:
///
/// (`BootRecord*` is short for the structs [`BootRecordCommon`],
/// [`BootRecord12_16`] and [`BootRecord32`]).
///
/// ```text
/// byte            FAT12/FAT16                          FAT32
///      ┌───────────────────────────────┐ ┌───────────────────────────────┐
/// 0x00 │    BootRecord*::jump_boot     │ │    BootRecord*::jump_boot     │
/// 0x03 │     BootRecord*::oem_name     │ │     BootRecord*::oem_name     │
///  ... │┌─────────────────────────────┐│ │┌┬───────────────────────────┬┐│
/// 0x0B ││          BpbCommon          ││ │││         BpbCommon         │││
///  ... ││             ...             ││ │││            ...            │││
///  ... │├─────────────────────────────┤│ ││└───────────────────────────┘││
/// 0x24 ││  BootRecordTail::drive_num  ││ ││    Bpb32::fat_lba_size32    ││
/// 0x25 ││  BootRecordTail::_reserved  ││ ││             ...             ││
///      ││┌───────────────────────────┐││ ││                             ││
/// 0x26 │││     BootRecordTailExt     │││ ││                             ││
///  ... |||            ...            ||| ││             ...             ││
/// 0x34 └┴┴───────────────────────────┴┴┘ ││      Bpb32::_reserved       ││
///  ...           (end at 0x3E)           │├─────────────────────────────┤│
/// 0x40                                   ││  BootRecordTail::drive_num  ││
/// 0x41                                   ││  BootRecordTail::_reserved  ││
///                                        ││┌───────────────────────────┐││
/// 0x42                                   │││     BootRecordTailExt     │││
///  ...                                   │││            ...            │││
///                                        └┴┴───────────────────────────┴┴┘
///                                                   (end at 0x5A)
/// ```
///
/// Note that [`BootRecordTailExt`] is not allowed to be missing according to
/// the EFI FAT specification, but it does have its own signature.
///
/// [`BootRecordCommon`]: boot_reg::BootRecordCommon
/// [`BootRecord12_16`]: boot_reg::BootRecord12_16
/// [`BootRecord32`]: boot_reg::BootRecord32
/// [`BootRecordTailExt`]: boot_reg::BootRecordTailExt
pub mod boot_reg {
	use super::*;
	use crate::Lba;

	pub const PRIMARY_LBA: Lba = 0;
	pub const BACKUP_LBA: Lba = 6;
	/// When creating a backup of the start of the partition, not just the first
	/// block should be copied. This is the number of blocks that are backed up.
	pub const BOOT_REG_LBA_SIZE: Lba = 3;

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecordCommon {
		pub jump_boot: [u8; 3],
		pub oem_name: [u8; 8],
		pub bpb: BpbCommon,
		pub _rest: [u8; 0x01DA],
		pub signature: [u8; 2],
	}

	impl BootRecordCommon {
		pub const SIGNATURE: [u8; 2] = 0xAA55_u16.to_le_bytes();
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecord12_16 {
		pub jump_boot: [u8; 3],
		pub oem_name: [u8; 8],
		pub bpb: BpbCommon,
		pub tail: BootRecordTail,
		pub _boot_code: [u8; 0x01C0],
		pub signature: [u8; 2],
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecord32 {
		pub jump_boot: [u8; 3],
		pub oem_name: [u8; 8],
		pub bpb: Bpb32,
		pub tail: BootRecordTail,
		pub _boot_code: [u8; 0x01A4],
		pub signature: [u8; 2],
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
	)]
	#[repr(C)]
	pub struct BpbCommon {
		pub blk_size: U16Le,
		pub cluster_lba_size: u8,
		pub fat_base_lba: U16Le,
		pub fat_count: u8,
		pub root_capacity: U16Le,
		pub volume_lba_size16: U16Le,
		pub medium_type: u8,
		pub fat_lba_size16: U16Le,
		pub sectors_per_track: U16Le,
		pub heads: U16Le,
		pub partition_lba_offset: U32Le,
		pub volume_lba_size32: U32Le,
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
	)]
	#[repr(C)]
	pub struct Bpb32 {
		pub common: BpbCommon,
		pub fat_lba_size32: U32Le,
		pub ext_flags: U16Le,
		pub version: U16Le,
		pub first_root_cluster: U32Le,
		pub fsinfo_lba: U16Le,
		pub backup_lba: U16Le,
		pub _reserved: [u8; 0xC],
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecordTail {
		pub drive_num: u8,
		pub _reserved: u8,
		pub ext: BootRecordTailExt,
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecordTailExt {
		pub signature: u8,
		pub id: U32Le,
		pub label: ShortName,
		pub fat_type_name: [u8; 8],
	}

	impl BootRecordTailExt {
		pub const SIGNATURE: u8 = 0x29;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
	)]
	#[repr(C)]
	pub struct FsInfo {
		pub lead_signature: U32Le,
		pub _reserved0: [u8; 0x01E0],
		pub struct_signature: U32Le,
		pub free_count: U32Le,
		pub next_free: U32Le,
		pub _reserved1: [u8; 0xC],
		pub trail_signature: U32Le,
	}

	impl FsInfo {
		pub const DEFAULT_LBA: Lba = 1;

		pub const LEAD_SIGNATURE: U32Le = *b"RRaA";
		pub const STRUCT_SIGNATURE: U32Le = *b"rrAa";
		pub const TRAIL_SIGNATURE: U32Le = 0xAA55_0000_u32.to_le_bytes();
	}
}

#[derive(Clone)]
#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
)]
#[repr(C)]
pub struct GenericEntry {
	pub name_byte0: u8,
	pub _custom0: [u8; 0xA],
	pub attributes: Attributes,
	pub _custom1: [u8; 0x14],
}

impl GenericEntry {
	pub const BYTE0_END: u8 = 0;
	pub const BYTE0_SIDESTEP_VACANT: u8 = 5;
	pub const BYTE0_VACANT: u8 = 0xE5;
}

#[derive(Clone)]
#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
)]
#[repr(C)]
pub struct ChildEntry {
	pub name: ShortName,
	pub attributes: Attributes,
	pub _reserved: u8,
	/// The spec says that this counts tenths of a second but it is clearly wrong.
	pub creation_time_10ms: u8,
	pub creation_time: U16Le,
	pub creation_date: U16Le,
	pub access_date: U16Le,
	pub first_cluster_hi: U16Le,
	pub write_time: U16Le,
	pub write_date: U16Le,
	pub first_cluster_lo: U16Le,
	pub data_size: U32Le,
}

#[derive(Clone)]
#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
)]
#[repr(C)]
pub struct LongNameEntry {
	pub ord: u8,
	pub name0: [u8; 0xA],
	pub attributes: Attributes,
	pub component_type: u8,
	pub checksum: u8,
	pub name1: [u8; 0xC],
	pub _zero: [u8; 2],
	pub name2: [u8; 4],
}

impl LongNameEntry {
	/// Number of WTF-16 code units in one component.
	pub const CHAR_COUNT: usize = 0xD;
	/// The only possible value of `component_type`.
	pub const TYPE_CHILD: u8 = 0;
}

/// Attributes of a file.
///
/// The behavior of [`Attributes::all`]/[`Attributes::from_bits_truncate`] is
/// stable.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "bytemuck", derive(Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
)]
#[repr(transparent)]
pub struct Attributes(u8);

bitflags! {
	impl Attributes: u8 {
		/// Indicates that writes to the file should fail.
		const READ_ONLY = 1;
		/// Indicates that normal directory listings should not show this file.
		const HIDDEN = 2;
		/// Indicates that this is an operating system file.
		const SYSTEM = 4;
		/// There should only be one "file" on the volume that has this
		/// attribute set, and that file must be in the root directory.
		///
		/// This name of this file is actually the label for the volume.
		/// The first cluster must always be 0 for the volume label
		/// (no data clusters are allocated to the volume label file).
		const VOLUME_ID = 8;
		/// Indicates that this file is actually a directory.
		const DIR = 0x10;
		/// This attribute supports backup utilities.
		///
		/// This bit is set by the FAT file system driver when a file is
		/// created, renamed, or written to.
		/// Backup utilities may use this attribute to indicate which files on
		/// the volume have been modified since the last time that a backup was
		/// performed.
		const ARCHIVE = 0x20;
		/// Indicates that this file is actually part of a long file name.
		const LONG_NAME = 0x0F;
	}
}

/// Computes the checksum of a short name.
///
/// # Examples
/// ```
/// # use fat_fs_types::fat::{name_checksum, NO_NAME, PARENT_DIR, SELF_DIR};
/// assert_eq!(name_checksum(&NO_NAME), 0xB3);
/// assert_eq!(name_checksum(&SELF_DIR), 0x77);
/// assert_eq!(name_checksum(&PARENT_DIR), 0xC2);
/// ```
pub fn name_checksum(name: &ShortName) -> u8 {
	name.iter()
		.fold(0, |sum, &byte| u8::wrapping_add(sum.rotate_right(1), byte))
}

/// Whether or not a char is valid in an ASCII short name (returns `false` for
/// non-ASCII characters).
///
/// `.`, lower-case ASCII characters and [`GenericEntry::BYTE0_SIDESTEP_VACANT`]
/// are not considered valid.
///
/// # Examples
/// ```
/// # use fat_fs_types::fat::is_valid_ascii_short_char;
/// for b in 0..=0xFF {
///     assert_eq!(
///         is_valid_ascii_short_char(b),
///         !matches!(
///             b,
///             (..=0x1F) | b'"' | b'*'..=b',' | b'.' | b'/' | b':'..=b'?' | b'['..=b']'
///             | b'a'..=b'z' | b'|' | 0x80..
///         ),
///     );
/// }
/// ```
pub fn is_valid_ascii_short_char(b: u8) -> bool {
	static VALID: [u8; 0x20] = [
		0x00, 0x00, 0x00, 0x00, 0xFB, 0x23, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0xC7, 0x01, 0x00, 0x00,
		0xE8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
		0x00, 0x00,
	];

	VALID[b as usize >> 3] & 1_u8.wrapping_shl(b as u32) != 0
}

/// Whether or not a char is valid in a long name.
///
/// # Examples
/// ```
/// # use fat_fs_types::fat::is_valid_long_char;
/// for c in 0..0x80 {
///     assert_eq!(
///         is_valid_long_char(c),
///         !matches!(
///             c as u8,
///             (..=0x1F) | b'"' | b'*' | b'/' | b':' | b'<' | b'>' | b'?' | b'\\' | b'|'
///         ),
///     );
/// }
/// // all non-ASCII code points are valid
/// assert!(is_valid_long_char(0x80));
/// ```
pub fn is_valid_long_char(b: u16) -> bool {
	static VALID: [u8; 0x10] = [
		0x00, 0x00, 0x00, 0x00, 0xFB, 0x7B, 0xFF, 0x2B, 0xFF, 0xFF, 0xFF, 0xEF, 0xFF, 0xFF, 0xFF,
		0xEF,
	];

	*VALID.get(b as usize >> 3).unwrap_or(&0xFF) & 1_u8.wrapping_shl(b as u32) != 0
}

/// Encodes `(hours, minutes, seconds)` into a packed native-endian FAT write
/// time.
///
/// The result will not represent a valid time if the input was invalid.
///
/// # Examples
/// ```
/// # use fat_fs_types::fat::pack_write_time;
/// assert_eq!(pack_write_time((17, 29, 3)), 0x8BA1);
/// ```
pub const fn pack_write_time((hours, minutes, seconds): (u8, u8, u8)) -> u16 {
	(hours as u16) << 0xB | (minutes as u16) << 5 | seconds as u16 >> 1
}

/// Decodes a packed native-endian FAT write time into
/// `(hours, minutes, seconds)`.
///
/// The result might not be a valid time.
///
/// # Examples
/// ```
/// # use fat_fs_types::fat::unpack_write_time;
/// assert_eq!(unpack_write_time(0x8BA1), (17, 29, 2));
/// ```
pub const fn unpack_write_time(packed: u16) -> (u8, u8, u8) {
	(
		(packed >> 0xB) as u8,
		(packed >> 5) as u8 & 0x3F,
		(packed as u8 & 0x1F) << 1,
	)
}
