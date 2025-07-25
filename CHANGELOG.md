# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/ "null").

## [1.0.0] - 2025-07-25

### Added

- Initial release of `cl-win32-errors`.
  
- A comprehensive mapping of over 2,300 Windows API error codes to their symbolic names and descriptions.
  
- Core API functions (`get-error-details`, `format-error-message`) for error code translation.
  
- A `win32-error` condition system with accessors for idiomatic Lisp error handling.
  
- ASDF system definitions for the library (`cl-win32-errors`) and its test suite (`cl-win32-errors/tests`).
  
- Full unit test suite using FiveAM to verify API correctness and data integrity.