# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Added

- Introduced ability to add params inside given in pact interaction
- Introduced basic pact matchers
- Introduced support for writing consumer/provider message pact tests
- Introduced support for pact verification

### Fixed

- Pact verification escript now locates the compiled `pact_erlang` beams
  relative to its own location, so verification works when the current working
  directory is not the project root (e.g. under Common Test)
- Pact verification escript arguments are now passed and quoted individually,
  so empty arguments (e.g. an unset state change url) no longer disappear and
  shift every following argument by one


## [0.2.2] - 2023-11-03

### Fixed
- Support for missing linux aarch64 architecture


## [0.2.1] - 2023-08-17

### Added

- Introduced pact:write/1 variant (#17).


## [0.2.0] - 2023-08-15

Major change in the API (#11).

Supports:
- Consumer tests
- Specifying provider states


## [0.1.2] - 2023-06-29

### Added

- Function to get mock server mismatches


## [0.1.1] - 2023-06-23

Initial POC with basic support.
