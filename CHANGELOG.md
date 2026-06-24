# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/).

## [Unreleased]

## [0.3.2] - 2026-06-24

### Fixed

- Backward compatibility fix for a breaking change introduced in [0.3.1].
- Compilation issues with older ubuntu versions.

### Added

- Support for verifying message pacts from service's own http endpoint.
- Alpine/musl Linux support: detect musl at build time and select the correct precompiled `libpact_ffi` library ([#61](https://github.com/greyorange-labs/pact_erlang/pull/61)).

## [0.3.1] - 2025-07-05

### Fixed

- **[Important Bugfix]** Unable to verify pacts ([#32](https://github.com/greyorange-labs/pact_erlang/issues/32))
- Fixes compiler warnings ([#45](https://github.com/greyorange-labs/pact_erlang/issues/45))
- Fixed consumer version selectors option, it was not getting adhered.  

### Changed

- Upgraded pact ffi version to 0.4.27. Note: Existing pacts in the pact broker may cause version conflicts due to schema changes. Users are advised to update their existing pacts to align with the new version or remove outdated pacts from the broker to avoid issues.
- **[Breaking Change]** `consumer_version_selectors` option in pact verification now expects a list of maps instead of a encoded json object.

### Added

- Pact interaction verification results will start getting printed to stdout.
- Ability to enable pact ffi library logging using `pact:enable_logging(LogLevel)`.
- Option to pass `publish_verification_results` in provider verification options, see README.md for usage.

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
