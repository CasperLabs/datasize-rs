# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed

* There are no longer `Sized` trait bounds on `T` on all `data_size` functions (thanks, @s3bk).
* Memory usage estimation for `HashSet` and `HashMap` has been improved, now reflects the actual hashbrown implementation used in the Rust stdlib (thanks, @SimonSapin).

## [0.2.8] - previous release

This (and previous) versions of `datasize-rs` did not include a `CHANGELOG.md`.
