#![doc = include_str!("../README.md")]
#![no_std]

pub mod exfat;
pub mod fat;

#[cfg(feature = "bytemuck")]
pub use bytemuck;
#[cfg(feature = "zerocopy")]
pub use zerocopy;

use core::{
	cmp::Ordering,
	fmt::{self, Debug, Display, Formatter},
};

#[cfg(feature = "bytemuck")]
use bytemuck::{Pod, Zeroable};
#[cfg(feature = "zerocopy")]
use zerocopy::{FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned};

pub const MIN_BLK_SIZE: usize = 0x0200;
pub const MAX_BLK_SIZE: usize = 0x1000;
/// Number of clusters that show up at the start of the FAT but don't correspond
/// to a heap allocation.
pub const NONEXISTENT_CLUSTERS: ClusterIdx = 2;

pub type ClusterIdx = u32;
/// A globally unique identifier (usually called universally unique
/// identifier/UUID outside the Microsoft world) with mixed endianness.
pub type Guid = [u8; 0x10];
/// Logical block addressing address or offset.
pub type Lba = u64;

/// Unaligned little-endian 16-bit integer.
pub type U16Le = [u8; 2];
/// Unaligned little-endian 32-bit integer.
pub type U32Le = [u8; 4];
/// Unaligned little-endian 64-bit integer.
pub type U64Le = [u8; 8];

/// A cylinder-head-sector address in the format used by x86's interrupt `0x13`.
///
/// # Examples
/// ```
/// # use fat_fs_types::ChsAddr;
/// let chs_addr = ChsAddr::new(0x0102, 3, 5).unwrap();
/// assert_eq!(chs_addr, ChsAddr([3, 0x45, 2]));
/// assert_eq!(chs_addr.cylinder(), 0x0102);
/// assert_eq!(chs_addr.head(), 3);
/// assert_eq!(chs_addr.sector(), 5);
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "bytemuck", derive(Pod, Zeroable))]
#[cfg_attr(
	feature = "zerocopy",
	derive(FromBytes, Immutable, IntoBytes, KnownLayout, Unaligned)
)]
#[repr(C)]
pub struct ChsAddr(pub [u8; 3]);

impl ChsAddr {
	/// An invalid all-zero cylinder-head-sector address.
	pub const ZERO: Self = Self([0; 3]);
	/// An invalid all-one cylinder-head-sector address.
	pub const INVALID: Self = Self([0xFF; 3]);

	/// Creates a new cylinder-head-sector address, returning `None` if
	/// the values `cylinder` or `sector` are too large.
	#[inline]
	pub const fn new(cylinder: u16, head: u8, sector: u8) -> Option<Self> {
		if cylinder < 0x0400 && sector < 0x40 {
			Some(Self([
				head,
				sector | ((cylinder >> 2) as u8 & 0xC0),
				cylinder as u8,
			]))
		} else {
			None
		}
	}

	#[inline]
	pub const fn cylinder(self) -> u16 {
		((self.0[1] as u16 & 0xC0) << 2) | self.0[2] as u16
	}
	#[inline]
	pub const fn head(self) -> u8 {
		self.0[0]
	}
	#[inline]
	pub const fn sector(self) -> u8 {
		self.0[1] & 0x3F
	}
}

impl Debug for ChsAddr {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		f.debug_struct("ChsAddr")
			.field("cylinder", &self.cylinder())
			.field("head", &self.head())
			.field("sector", &self.sector())
			.finish()
	}
}

impl PartialOrd for ChsAddr {
	fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
		Some(Self::cmp(self, other))
	}
}
impl Ord for ChsAddr {
	fn cmp(&self, other: &Self) -> Ordering {
		Ord::cmp(
			&(self.cylinder(), self.head(), self.0[1]),
			&(other.cylinder(), other.head(), other.0[1]),
		)
	}
}

/// Hard disk geometry values used by x86's interrupt `0x13`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct DiskGeometry {
	pub heads: u16,
	pub sectors_per_track: u16,
}

impl DiskGeometry {
	/// Converts a logical block address to a cylinder-head-sector address.
	///
	/// # Examples
	/// ```
	/// # use fat_fs_types::{ChsAddr, DiskGeometry};
	/// let geometry = DiskGeometry {
	///     heads: 200,
	///     sectors_per_track: 10,
	/// };
	/// assert_eq!(geometry.lba_to_chs(0), ChsAddr::new(0, 0, 1));
	/// assert_eq!(geometry.lba_to_chs(0x1234), ChsAddr::new(2, 66, 1));
	/// assert_eq!(geometry.lba_to_chs(99_999), ChsAddr::new(49, 199, 10));
	/// ```
	pub const fn lba_to_chs(self, lba: Lba) -> Option<ChsAddr> {
		let sector = (lba % self.sectors_per_track as u64) as u8 + 1;
		let head_and_cylinder = lba / self.sectors_per_track as u64;
		let head = (head_and_cylinder % self.heads as u64) as u8;
		let cylinder = head_and_cylinder / self.heads as u64;
		if cylinder > u16::MAX as u64 {
			return None;
		}
		ChsAddr::new(cylinder as u16, head, sector)
	}

	/// Converts a cylinder-head-sector address to a logical block address.
	///
	/// For valid inputs, the result is always smaller than
	/// `0x0400 * heads * sectors_per_track`.
	///
	/// # Examples
	/// ```
	/// # use fat_fs_types::{ChsAddr, DiskGeometry};
	/// let geometry = DiskGeometry {
	///     heads: 200,
	///     sectors_per_track: 10,
	/// };
	/// assert_eq!(geometry.chs_to_lba(ChsAddr::new(0, 0, 1).unwrap()), 0);
	/// assert_eq!(
	///     geometry.chs_to_lba(ChsAddr::new(2, 66, 1).unwrap()),
	///     0x1234
	/// );
	/// assert_eq!(
	///     geometry.chs_to_lba(ChsAddr::new(49, 199, 10).unwrap()),
	///     99_999
	/// );
	/// ```
	pub const fn chs_to_lba(self, chs: ChsAddr) -> Lba {
		(chs.cylinder() as u64 * self.heads as u64 + chs.head() as u64)
			* self.sectors_per_track as u64
			+ chs.sector() as u64
			- 1
	}
}

pub const MIN_YEAR: i32 = 1980;
pub const MAX_YEAR: i32 = 2107;

/// Encodes `(year, month, day)` into a packed native-endian FAT/exFAT date.
///
/// The result will not represent a valid date if the input has an invalid month
/// or day component.
///
/// # Errors
/// Throws if the year lies outside the valid range.
///
/// # Examples
/// ```
/// # use fat_fs_types::{pack_date, pack_time, MAX_YEAR};
/// assert_eq!(pack_date((2015, 5, 15)).unwrap(), 0x46AF);
///
/// // this overflows and the error gives you the maximum valid datetime via the
/// // `saturated_*` methods
/// let e = pack_date((2177, 5, 15)).unwrap_err();
/// assert!(e.overflow());
/// let (max_time, max_time_10ms) = pack_time((23, 59, 59_990));
/// assert_eq!(
///     (e.saturating_date(), e.saturating_time(), e.saturating_time_10ms()),
///     (pack_date((MAX_YEAR, 12, 31)).unwrap(), max_time, max_time_10ms),
/// );
/// ```
pub const fn pack_date((year, month, day): (i32, u8, u8)) -> Result<u16, PackDateError> {
	if year < MIN_YEAR {
		return Err(PackDateError { overflow: false });
	}
	let year = year - MIN_YEAR;
	if year >= 0x80 {
		return Err(PackDateError { overflow: true });
	}
	Ok((year as u16) << 9 | (month as u16) << 5 | day as u16)
}

/// Decodes a packed native-endian FAT/exFAT date into `(year, month, day)`.
///
/// The result might not be a valid date.
///
/// # Examples
/// ```
/// # use fat_fs_types::{unpack_date, MAX_YEAR, MIN_YEAR};
/// assert_eq!(unpack_date(0), (MIN_YEAR, 0, 0));
/// assert_eq!(unpack_date(0x46AF), (2015, 5, 15));
/// assert_eq!(unpack_date(u16::MAX), (MAX_YEAR, 15, 31));
/// ```
pub const fn unpack_date(packed: u16) -> (i32, u8, u8) {
	(
		MIN_YEAR + (packed >> 9) as i32,
		(packed >> 5) as u8 & 0x0F,
		packed as u8 & 0x1F,
	)
}

/// Error thrown by [`pack_date`].
#[derive(Clone, Copy, Debug)]
pub struct PackDateError {
	overflow: bool,
}

impl PackDateError {
	/// Whether an overflow was the cause, as opposed to an underflow.
	#[inline]
	pub const fn overflow(self) -> bool {
		self.overflow
	}
	/// Whether an underflow was the cause, as opposed to an overflow.
	#[inline]
	pub const fn underflow(self) -> bool {
		!self.overflow
	}
	#[inline]
	pub const fn saturating_date(self) -> u16 {
		if self.overflow {
			0xFF9F
		} else {
			0x21
		}
	}
	#[inline]
	pub const fn saturating_time(self) -> u16 {
		if self.overflow {
			0xBF7D
		} else {
			0
		}
	}
	#[inline]
	pub const fn saturating_time_10ms(self) -> u8 {
		if self.overflow {
			199
		} else {
			0
		}
	}
}

impl Display for PackDateError {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		f.write_str("FAT date packing error: ")?;
		f.write_str(if self.overflow {
			"overflow"
		} else {
			"underflow"
		})
	}
}

impl core::error::Error for PackDateError {}

/// Encodes `(hours, minutes, millis)` into a packed native-endian FAT/exFAT
/// time, rounding down.
///
/// For the write time field of the FAT filesystem or the access time field of
/// exFAT, use [`fat::pack_write_time`] or [`exfat::pack_access_time`] instead.
///
/// The result will not represent a valid time if the input was invalid.
///
/// # Examples
/// ```
/// # use fat_fs_types::pack_time;
/// assert_eq!(pack_time((17, 29, 3_141)), (0x8BA1, 114));
/// ```
pub const fn pack_time((hours, minutes, millis): (u8, u8, u16)) -> (u16, u8) {
	(
		(hours as u16) << 0xB | (minutes as u16) << 5 | (millis / 2_000),
		(millis / 10 % 200) as u8,
	)
}

/// Decodes a packed native-endian FAT/exFAT time into
/// `(hours, minutes, millis)`.
///
/// For the write time field of the FAT filesystem or the access time field of
/// exFAT, use [`fat::unpack_write_time`] or [`exfat::unpack_access_time`]
/// instead.
///
/// The result might not be a valid time.
///
/// # Examples
/// ```
/// # use fat_fs_types::unpack_time;
/// assert_eq!(unpack_time(0x8BA1, 114), (17, 29, 3_140));
/// ```
pub const fn unpack_time(packed: u16, packed_10ms: u8) -> (u8, u8, u16) {
	(
		(packed >> 0xB) as u8,
		(packed >> 5) as u8 & 0x3F,
		(packed & 0x1F) * 2_000 + packed_10ms as u16 * 10,
	)
}
