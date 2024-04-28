# Rust crate `fat_fs_types`

Low-level `#![no_std]` helper library for accessing Microsoft's FAT12/16/32 and exFAT file systems.
Only defines constants, types and some very simple functions.

You will want to use one of the features `zerocopy` or `bytemuck` (together with the respective crate).

A lot of the structs in here only implement `Copy` if you have `feature = "bytemuck"` enabled since some of them are quite large.
All `#[repr(C)]` structs with public fields have a stable layout, no padding and an alignment of 1.

- EFI FAT specification: [download](https://download.microsoft.com/download/1/6/1/161ba512-40e2-4cc9-843a-923143f3456c/fatgen103.doc)
- [exFAT specification](https://learn.microsoft.com/en-us/windows/win32/fileio/exfat-specification)
