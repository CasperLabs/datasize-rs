# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [UNRELEASED]

## Added

* Support for arbitrary arrays using const generics has been added under the `const-generics` feature.
* Support for `Box<[T: DataSize]>` and `Box<str>` has been added.
* Support for structs with default values in generic parameters has been added.
* Support for `AssertUnwindSafe<T>` and `Reverse<T>` has been added.
* Support for `BinaryHeap` has been added.

## [0.2.13] - 2022-12-27

Replaces `0.2.12`, which was yanked.

## Fixed

Refer to the correction of the `datasize_derive` crate in `datasize` dependencies.

## [0.2.12] - 2022-12-27 (YANKED)

### Changed

* There are no longer `Sized` trait bounds on `T` on all `data_size` functions (thanks, @s3bk).
* Memory usage estimation for `HashSet` and `HashMap` has been improved, now reflects the actual hashbrown implementation used in the Rust stdlib (thanks, @SimonSapin).

## [0.2.11] - 2022-04-11

### Fixed

* An enum variant with no fields will no longer cause a compilation failiure when deriving `DataSize`.

## [0.2.10] - previous release

This (and previous) versions of `datasize-rs` did not include a `CHANGELOG.md`.
