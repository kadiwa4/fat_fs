//! exFAT.

pub use crate::fat::{
	is_valid_long_char as is_valid_char, pack_write_time as pack_access_time,
	unpack_write_time as unpack_access_time,
};

use core::hash::Hasher;

use bitflags::bitflags;
#[cfg(feature = "bytemuck")]
use bytemuck::{Pod, Zeroable};
#[cfg(feature = "zerocopy")]
use zerocopy::{AsBytes, FromBytes, FromZeroes, Unaligned};

use crate::{ClusterIdx, Guid, U16Le, U32Le, U64Le};

pub const MAX_CLUSTER_SIZE: usize = 0x0200_0000;
/// Maximum FAT length.
///
/// **Note:** This is not the same as the cluster count; this is the number of
/// entries in a FAT and not the number of clusters on the heap.
///
/// The value of this limit is phrased ambiguously in the specification:
///
/// > ClusterCount + 1 can never exceed FFFFFFF6h.
///
/// They probably intended to say `ClusterCount + 1 < 0xFFFF_FFF6`
/// (as opposed to `<=`). Otherwise the last FAT entry would clash with the
/// [`BAD_CLUSTER_MARK`].
pub const MAX_FAT_LEN: ClusterIdx = 0xFFFF_FFF6;
pub const MAX_NAME_LEN: usize = 0xFF;
/// Special value in the upcase table that indicates that the next n values
/// are all identity mappings, where n is the next 2-byte word.
///
/// Note that this value can also show up at the end of the table and there
/// it should be interpreted literally as `0xFFFF`.
pub const UPCASE_TABLE_MASS_IDENTITY_MARK: u16 = 0xFFFF;

pub const FAT_ENTRY0: ClusterIdx = 0xFFFF_FFF8;
pub const FAT_ENTRY1: ClusterIdx = 0xFFFF_FFFF;
pub const BAD_CLUSTER_MARK: ClusterIdx = 0xFFFF_FFF7;
pub const END_OF_ALLOC_MARK: ClusterIdx = 0xFFFF_FFFF;

#[derive(Clone)]
#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(AsBytes, FromZeroes, FromBytes, Unaligned)
)]
#[repr(C)]
pub struct BootRecord {
	pub jump_boot: [u8; 3],
	pub fs_name: [u8; 8],
	pub _zero: [u8; 0x35],
	pub partition_lba_offset: U64Le,
	pub volume_lba_size: U64Le,
	pub fat_base_lba: U32Le,
	pub fat_lba_size: U32Le,
	pub heap_base_lba: U32Le,
	pub heap_lba_size: U32Le,
	pub first_root_cluster: U32Le,
	pub volume_serial_number: U32Le,
	pub version: [u8; 2],
	pub flags: VolumeFlags,
	pub blk_size_log2: u8,
	pub cluster_lba_size_log2: u8,
	pub fat_count: u8,
	pub drive_num: u8,
	pub percent_occupied: u8,
	pub _reserved: [u8; 7],
	pub boot_code: [u8; 0x0186],
	pub signature: [u8; 2],
}

impl BootRecord {
	// offsets of struct fields
	const FLAGS_OFFSET: usize = 0x6A;
	const BLK_SIZE_LOG2_OFFSET: usize = 0x6C;
	const PERCENT_OCCUPIED_OFFSET: usize = 0x70;
	const RESERVED_OFFSET: usize = 0x71;

	pub const JUMP_BOOT: [u8; 3] = [0xEB, 0x76, 0x90];
	pub const FS_NAME: [u8; 8] = *b"EXFAT   ";
	pub const SIGNATURE: [u8; 2] = 0xAA55_u16.to_le_bytes();
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "bytemuck", derive(Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(AsBytes, FromZeroes, FromBytes, Unaligned)
)]
#[repr(C, packed)]
pub struct VolumeFlags(u16);

bitflags! {
	impl VolumeFlags: u16 {
		const FAT1_ACTIVE = 1_u16.to_le();
		const DIRTY = 2_u16.to_le();
		const IO_ERROR = 4_u16.to_le();
		const CLEAR_BEFORE_WRITE = 8_u16.to_le();
	}
}

#[derive(Clone, Default)]
#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(AsBytes, FromZeroes, FromBytes, Unaligned)
)]
#[repr(C)]
pub struct GenericParams {
	pub guid: Guid,
	pub _custom: [u8; 0x20],
}

impl GenericParams {
	pub const NULL: Self = Self {
		guid: [0; 0x10],
		_custom: [0; 0x20],
	};
}

#[derive(Clone)]
#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(AsBytes, FromZeroes, FromBytes, Unaligned)
)]
#[repr(C)]
pub struct FlashParams {
	pub guid: Guid,
	pub erase_blk_size: U32Le,
	pub page_size: U32Le,
	pub spare_blk_count: U32Le,
	pub random_access_time_ns: U32Le,
	pub programming_time_ns: U32Le,
	pub read_cycle_ns: U32Le,
	pub write_cycle_ns: U32Le,
	pub _reserved: [u8; 4],
}

impl FlashParams {
	pub const GUID: Guid = [
		0x46, 0x7E, 0x0C, 0x0A, 0x99, 0x33, 0x21, 0x40, 0x90, 0xC8, 0xFA, 0x6D, 0x38, 0x9C, 0x4B,
		0xA2,
	];
}

#[derive(Clone, Debug, Default)]
pub struct Checksum(u32);

impl Checksum {
	pub const NEW: Self = Self(0);

	/// Writes one or more blocks from the boot region into this hasher, the
	/// first of which being the main boot block.
	///
	/// `buf` has too be a multiple of one block in size.
	pub fn write_main_boot_block(&mut self, buf: &[u8]) {
		self.write(&buf[..BootRecord::FLAGS_OFFSET]);
		self.write(&buf[BootRecord::BLK_SIZE_LOG2_OFFSET..BootRecord::PERCENT_OCCUPIED_OFFSET]);
		self.write(&buf[BootRecord::RESERVED_OFFSET..]);
	}

	/// Return the computed checksum.
	#[inline]
	pub fn finalize(self) -> u32 {
		self.0
	}
}

impl Hasher for Checksum {
	#[inline]
	fn finish(&self) -> u64 {
		self.0 as u64
	}

	fn write(&mut self, bytes: &[u8]) {
		self.0 = bytes.iter().fold(self.0, |sum, &byte| {
			u32::wrapping_add(sum.rotate_right(1), byte as u32)
		});
	}
}

/// Contents of a directory.
pub mod dir {
	use super::*;

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct GenericEntry {
		pub entry_type: u8,
		pub _custom: [u8; 0x13],
		pub first_cluster: U32Le,
		pub data_capacity: U64Le,
	}

	impl GenericEntry {
		pub const TYPE_END: u8 = 0;
		pub const TYPE_BENIGN_FLAG: u8 = 0x20;
		pub const TYPE_SECONDARY_FLAG: u8 = 0x40;
		pub const TYPE_OCCUPIED_FLAG: u8 = 0x80;
		pub const TYPE_FLAGS: u8 = 0xE0;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct PrimaryEntry {
		pub entry_type: u8,
		pub secondary_count: u8,
		pub set_checksum: U16Le,
		pub flags: PrimaryEntryFlags,
		pub _custom: [u8; 0xE],
		pub first_cluster: U32Le,
		pub data_capacity: U64Le,
	}

	#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
	#[cfg_attr(feature = "bytemuck", derive(Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C, packed)]
	pub struct PrimaryEntryFlags(u16);

	bitflags! {
		impl PrimaryEntryFlags: u16 {
			const ALLOC_POSSIBLE = 1_u16.to_le();
			const NO_CLUSTER_CHAIN = 2_u16.to_le();
		}
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct AllocBitmapEntry {
		pub entry_type: u8,
		pub flags: u8,
		pub _reserved: [u8; 0x12],
		pub first_cluster: U32Le,
		pub data_capacity: U64Le,
	}

	impl AllocBitmapEntry {
		pub const TYPE: u8 = 0x81;
	}

	#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
	#[cfg_attr(feature = "bytemuck", derive(Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(transparent)]
	pub struct AllocBitmapEntryFlags(u8);

	bitflags! {
		impl AllocBitmapEntryFlags: u8 {
			const SECOND_FAT = 1;
		}
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct UpcaseTableEntry {
		pub entry_type: u8,
		pub _reserved0: [u8; 3],
		pub table_checksum: U32Le,
		pub _reserved1: [u8; 0xC],
		pub first_cluster: U32Le,
		pub data_capacity: U64Le,
	}

	impl UpcaseTableEntry {
		pub const TYPE: u8 = 0x82;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct VolumeLabelEntry {
		pub entry_type: u8,
		pub len: u8,
		pub label: [u8; 0x16],
		pub first_cluster: U32Le,
		pub data_capacity: U64Le,
	}

	impl VolumeLabelEntry {
		pub const TYPE: u8 = 0x83;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct ChildEntry {
		pub entry_type: u8,
		pub secondary_count: u8,
		pub set_checksum: U16Le,
		pub attributes: Attributes,
		pub _reserved0: [u8; 3],
		pub create_time: U16Le,
		pub create_date: U16Le,
		pub write_time: U16Le,
		pub write_date: U16Le,
		pub access_time: U16Le,
		pub access_date: U16Le,
		pub create_time_10ms: u8,
		pub write_time_10ms: u8,
		pub create_utc_offset: u8,
		pub write_utc_offset: u8,
		pub access_utc_offset: u8,
		pub _reserved1: [u8; 7],
	}

	impl ChildEntry {
		pub const TYPE: u8 = 0x85;
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
	#[repr(C, packed)]
	pub struct Attributes(u16);

	bitflags! {
		impl Attributes: u16 {
			/// Indicates that writes to the file should fail.
			const READ_ONLY = 1_u16.to_le();
			/// Indicates that normal directory listings should not show this file.
			const HIDDEN = 2_u16.to_le();
			/// Indicates that this is an operating system file.
			const SYSTEM = 4_u16.to_le();
			/// Indicates that this file is actually a directory.
			const DIR = 0x10_u16.to_le();
			/// This attribute supports backup utilities.
			const ARCHIVE = 0x20_u16.to_le();
		}
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct VolumeGuidEntry {
		pub entry_type: u8,
		pub secondary_count: u8,
		pub set_checksum: U16Le,
		pub flags: PrimaryEntryFlags,
		pub guid: Guid,
		pub _reserved1: [u8; 0xA],
	}

	impl VolumeGuidEntry {
		pub const TYPE: u8 = 0xA0;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct SecondaryEntry {
		pub entry_type: u8,
		pub flags: SecondaryEntryFlags,
		pub _custom: [u8; 0x12],
		pub first_cluster: U32Le,
		pub data_capacity: U64Le,
	}

	#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
	#[cfg_attr(feature = "bytemuck", derive(Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(transparent)]
	pub struct SecondaryEntryFlags(u8);

	bitflags! {
		impl SecondaryEntryFlags: u8 {
			const ALLOC_POSSIBLE = 1;
			const NO_CLUSTER_CHAIN = 2;
		}
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct StreamExtEntry {
		pub entry_type: u8,
		pub flags: SecondaryEntryFlags,
		pub _reserved0: u8,
		pub name_len: u8,
		pub name_hash: U16Le,
		pub _reserved1: [u8; 2],
		pub data_size: U64Le,
		pub _reserved2: [u8; 4],
		pub first_cluster: U32Le,
		pub data_capacity: U64Le,
	}

	impl StreamExtEntry {
		pub const TYPE: u8 = 0xC0;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct NameEntry {
		pub entry_type: u8,
		pub flags: SecondaryEntryFlags,
		pub buf: [u8; NameEntry::CAPACITY * 2],
	}

	impl NameEntry {
		pub const TYPE: u8 = 0xC1;
		pub const CAPACITY: usize = 0xF;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct VendorExtEntry {
		pub entry_type: u8,
		pub flags: SecondaryEntryFlags,
		pub vendor_guid: Guid,
		pub _custom: [u8; 0xE],
	}

	impl VendorExtEntry {
		pub const TYPE: u8 = 0xE0;
	}

	#[derive(Clone)]
	#[cfg_attr(feature = "bytemuck", derive(Copy, Pod, Zeroable))]
	#[cfg_attr(
		feature = "zerocopy",
		derive(AsBytes, FromZeroes, FromBytes, Unaligned)
	)]
	#[repr(C)]
	pub struct VendorAllocEntry {
		pub entry_type: u8,
		pub flags: SecondaryEntryFlags,
		pub vendor_guid: Guid,
		pub _custom: [u8; 2],
		pub first_cluster: U32Le,
		pub data_capacity: U64Le,
	}

	impl VendorAllocEntry {
		pub const TYPE: u8 = 0xE1;
	}
}
