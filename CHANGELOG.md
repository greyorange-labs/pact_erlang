# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

### Fixed

- **[Important Bugfix]** Unable to verify pacts ([#32](https://github.com/greyorange-labs/pact_erlang/issues/32))
- Upgraded to pact ffi version 0.4.27. Note: Existing pacts in the pact broker may cause version conflicts due to schema changes. Users are advised to update their existing pacts to align with the new version or remove outdated pacts from the broker to avoid issues.

## [0.3.0] - 2025-06-05

### Added

- Introduced ability to add params inside given in pact interaction
- Introduced basic pact matchers
- Introduced support for writing consumer/provider message pact tests
- Introduced support for pact verification
- Added support for checking pact verification logs
- Added support for Erlang/OTP 27 and 28

### Deprecated

- Erlang/OTP 25 is still supported, but will be removed in the 0.4.0 release


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
