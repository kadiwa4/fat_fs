//! FAT12/16/32.

use bitflags::bitflags;
#[cfg(feature = "bytemuck")]
use bytemuck::{Pod, Zeroable};
#[cfg(feature = "zerocopy")]
use zerocopy::{AsBytes, FromBytes, FromZeroes, Unaligned};

use crate::{ClusterIdx, U16Le, U32Le};

pub const END_OF_ALLOC_MARK_COUNT: ClusterIdx = 8;
pub const MAX_CLUSTER_SIZE: usize = 0x8000;

/// Minimum and maximum FAT lengths for every FAT type.
///
/// **Note:** This is not the same as the cluster count; this is the number of
/// entries in a FAT and not the number of clusters on the heap.
pub mod fat_len {
	use crate::ClusterIdx;

	pub const FAT12_MAX: ClusterIdx = 0x0FF6;
	pub const FAT16_MIN: ClusterIdx = 0x0FF7;
	pub const FAT16_MAX: ClusterIdx = 0xFFF6;
	pub const FAT32_MIN: ClusterIdx = 0xFFF7;
	pub const FAT32_MAX: ClusterIdx = 0x0FFF_FFF6;
}

/// Flags placed in the FAT entry at index 1.
pub mod fat_entry1_flag {
	use crate::ClusterIdx;

	pub const FAT16_NO_IO_ERROR: ClusterIdx = 0x4000;
	pub const FAT16_CLEAN: ClusterIdx = 0x8000;
	pub const FAT32_NO_IO_ERROR: ClusterIdx = 0x0400_0000;
	pub const FAT32_CLEAN: ClusterIdx = 0x0800_0000;
}

pub const MAX_LONG_NAME_LEN: usize = 0x0100;
pub const MAX_LONG_PATH_LEN: usize = 0x0104;
pub static NO_NAME: ShortName = *b"NO NAME    ";
pub static SELF_DIR: ShortName = *b".          ";
pub static SUPER_DIR: ShortName = *b"..         ";

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
	pub const fn name(self) -> &'static [u8; 8] {
		match self {
			Self::Fat12 => b"FAT12   ",
			Self::Fat16 => b"FAT16   ",
			Self::Fat32 => b"FAT32   ",
		}
	}

	pub const fn bad_cluster_mark(self) -> ClusterIdx {
		match self {
			Self::Fat12 => 0x0FF7,
			Self::Fat16 => 0xFFF7,
			Self::Fat32 => 0x0FFF_FFF7,
		}
	}

	pub const fn fat_byte_size(self, entry_count: ClusterIdx) -> u32 {
		match self {
			Self::Fat12 => entry_count + (entry_count + 1) / 2,
			_ => entry_count << self as u8,
		}
	}
}

/// The "reserved region" of the FAT volume contains the boot record and FSInfo
/// structures.
///
/// Field names ending with `lba_size` indicate that the field counts the number
/// of blocks that something occupies.
///
/// # First block
///
/// The beginning of the first block is structured like this:
///
/// ```text
/// byte              FAT12/FAT16                              FAT32
///      +-----------------------------------+ +-----------------------------------+
/// 0x00 |         BootRecord start          | |         BootRecord start          |
///  ... |                ...                | |                ...                |
/// 0x20 |   BootRecord::volume_lba_size32   | |   BootRecord::volume_lba_size32   |
///      +-----------------------------------+ +-----------------------------------+
/// 0x24 |        BootRecordEnd start        | |       BootRecordExt32 start       |
/// 0x25 |     BootRecordEnd::_reserved      | |                ...                |
///      +-----------------------------------+ |                                   |
/// 0x26 | BootRecordEndExt (optional) start | |                                   |
///  ... |                ...                | |                                   |
///  ... |  BootRecordEndExt (optional) end  | |                                   |
/// 0x3E +-----------------------------------+ |                ...                |
///  ...                                       |        BootRecordExt32 end        |
///  ...                                       +-----------------------------------+
/// 0x40                                       |        BootRecordEnd start        |
/// 0x41                                       |     BootRecordEnd::_reserved      |
///                                            +-----------------------------------+
/// 0x42                                       | BootRecordEndExt (optional) start |
///  ...                                       |                ...                |
///  ...                                       |  BootRecordEndExt (optional) end  |
/// 0x5A                                       +-----------------------------------+
/// ```
///
/// Note that [`BootRecordEndExt`] is not actually allowed to be missing
/// according to the EFI FAT specification, but it does have its own signature.
///
/// [`BootRecordEndExt`]: reserved_reg::BootRecordEndExt
pub mod reserved_reg {
	use super::*;
	use crate::Lba;

	pub const PRIMARY_LBA: Lba = 0;
	pub const BACKUP_LBA: Lba = 6;

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecord {
		pub jump_boot: [u8; 3],
		pub oem_name: [u8; 8],
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
		pub rest: [u8; 0x01DA],
		pub signature: [u8; 2],
	}

	impl BootRecord {
		pub const SIGNATURE: [u8; 2] = 0xAA55_u16.to_le_bytes();
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecordExt32 {
		pub fat_lba_size32: U32Le,
		pub ext_flags: U16Le,
		pub version: U16Le,
		pub first_root_cluster: U32Le,
		pub fsinfo_lba: U16Le,
		pub backup_lba: U16Le,
		pub _reserved: [u8; 0xC],
		pub end: BootRecordEnd,
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecordEnd {
		pub drive_num: u8,
		pub _reserved: u8,
		pub ext: BootRecordEndExt,
		pub _padding: [u8; 2],
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct BootRecordEndExt {
		pub signature: u8,
		pub id: U32Le,
		pub label: ShortName,
		pub fat_type_name: [u8; 8],
	}

	impl BootRecordEndExt {
		pub const SIGNATURE: u8 = 0x29;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
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
		pub const LEAD_SIGNATURE: U32Le = *b"RRaA";
		pub const STRUCT_SIGNATURE: U32Le = *b"rrAa";
		pub const TRAIL_SIGNATURE: U32Le = 0xAA55_0000_u32.to_le_bytes();
	}
}

#[derive(Clone)]
#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(AsBytes, FromZeroes, FromBytes, Unaligned)
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

impl ChildEntry {
	pub const ENTRY_END: u8 = 0;
	pub const ENTRY_SIDESTEP_VACANT: u8 = 5;
	pub const ENTRY_VACANT: u8 = 0xE5;
}

#[derive(Clone)]
#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(AsBytes, FromZeroes, FromBytes, Unaligned)
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
	/// Only possible value of `component_type`.
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
	derive(AsBytes, FromZeroes, FromBytes, Unaligned)
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
/// # use fat_fs_types::fat::{name_checksum, NO_NAME, SELF_DIR, SUPER_DIR};
/// assert_eq!(name_checksum(&NO_NAME), 0xB3);
/// assert_eq!(name_checksum(&SELF_DIR), 0x77);
/// assert_eq!(name_checksum(&SUPER_DIR), 0xC2);
/// ```
pub fn name_checksum(name: &ShortName) -> u8 {
	name.iter()
		.fold(0, |sum, &byte| u8::wrapping_add(sum.rotate_right(1), byte))
}

/// Whether or not a char is valid in a short name under the ASCII code page.
///
/// # Examples
/// ```
/// # use fat_fs_types::fat::is_valid_ascii_short_char;
/// for b in 0..0x80 {
///     assert_eq!(
///         is_valid_ascii_short_char(b),
///         matches!(
///             b,
///             b' ' | b'!' | b'#'..=b')' | b'-' | b'0'..=b'9' | b'@'..=b'Z' | b'^'..=b'`' | b'{'
///             | b'}' | b'~'
///         ),
///     );
/// }
/// ```
pub fn is_valid_ascii_short_char(b: u8) -> bool {
	static VALID: [u8; 0x10] = [
		0x00, 0x00, 0x00, 0x00, 0xFB, 0x23, 0xFF, 0x03, 0xFF, 0xFF, 0xFF, 0xC7, 0x01, 0x00, 0x00,
		0x68,
	];

	*VALID.get(b as usize >> 3).unwrap_or(&0) & 1_u8.wrapping_shl(b as u32) != 0
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
///             (..=0x1F) | b'"' | b'*' | b'/' | b':' | b'<' | b'>' | b'?' | b'\\' | b'|' | 0x7F
///         ),
///     );
/// }
/// assert!(is_valid_long_char(0x80));
/// ```
pub fn is_valid_long_char(b: u16) -> bool {
	static VALID: [u8; 0x10] = [
		0x00, 0x00, 0x00, 0x00, 0xFB, 0x7B, 0xFF, 0x2B, 0xFF, 0xFF, 0xFF, 0xEF, 0xFF, 0xFF, 0xFF,
		0x6F,
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
