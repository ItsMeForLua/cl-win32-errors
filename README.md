# cl-win32-errors

A Common Lisp library for translating cryptic Windows API error codes into human-readable information. When working with CFFI, Windows functions often return numeric error codes. This library provides a simple interface to convert these codes into their symbolic names, hexadecimal values, and descriptive strings, complete with a condition system for error handling.

This project is the logical companion to [cl-win32-types](https://github.com/ItsMeForLua/cl-win32-types).

## Features

- **Error Mapping:** Translates over 2,300 system error codes across numerous categories, including core system, networking, RPC, security, Active Directory, DNS, and more.
  
- **Extensible:** The library is structured to make adding new error code definitions simple.
  
- **Condition System:** Provides a `win32-error` condition for idiomatic Lisp error handling.
  
- **User-Friendly API:** Simple functions to get error details or formatted strings.
  
- **ASDF System:** Ready for straightforward use via Quicklisp. The library and its tests are defined as separate systems.
  

## Installation (for Users)

Once available in a Quicklisp distribution, you can load the library with:

``` lisp
(ql:quickload :cl-win32-errors)
```

## Developer Setup (for Contributors)

This project uses [Qlot](https://github.com/fukamachi/qlot "null") to manage and lock dependency versions for reproducible builds.

1. **Install Qlot** (one-time setup):
  
  ``` powershell
  ros install qlot
  ```
  
2. Install Project Dependencies:
  
  This command reads the qfile and installs the exact versions of all dependencies into a local .qlot/ directory.
  
  ``` powershell
  # Make sure you are in the project's root directory
  qlot install
  ```
  
3. Start a REPL:
  
  To work on the project, start your REPL using qlot exec. This ensures your Lisp session uses the project's local dependencies.
  
  ``` powershell
  qlot exec ros run
  ```
  

## Quick Start / Usage Example

Here is the most common use case for the library. All functions are exported from the `cl-win32-errors` package.

``` lisp
;; Load the library
(ql:quickload :cl-win32-errors)

;; Look up an error code
(cl-win32-errors:get-error-details 2)
;; => (:CODE 2 :HEX "0x2" :SYMBOL :ERROR_FILE_NOT_FOUND
;;     :DESCRIPTION "The system cannot find the file specified.")

;; Get a formatted string directly
(cl-win32-errors:format-error-message 5 :verbosity :verbose)
;; => "Win32 Error 5 (0x5/ERROR_ACCESS_DENIED): Access is denied."
```

## API Reference

### `get-error-details` (error-code)

Looks up a numeric Win32 error code and returns a property list (plist) of its details.

**Parameters:**

- `error-code`: An integer representing the Windows error code.

**Returns:** A plist with the keys `:code`, `:hex`, `:symbol`, and `:description`, or `NIL` if the code is not found.

**Example:**

``` lisp
(get-error-details 5)
;; => (:CODE 5 :HEX "0x5" :SYMBOL :ERROR_ACCESS_DENIED :DESCRIPTION "Access is denied.")
```

### `format-error-message` (error-code &key (verbosity :default))

Formats a Win32 error code into a human-readable string.

**Parameters:**

- `error-code`: An integer representing the Windows error code.
  
- `:verbosity`: A keyword, either `:default` or `:verbose`. The `:verbose` option includes the hex code and symbolic name.
  

**Returns:** A formatted string.

**Example:**

``` lisp
(format-error-message 87 :verbosity :verbose)
;; => "Win32 Error 87 (0x57/ERROR_INVALID_PARAMETER): The parameter is incorrect."
```

## Condition System

The library provides a `win32-error` condition for idiomatic error handling. You can signal it after a failed API call and use the provided accessors to inspect its details within a `handler-case`.

**Condition Accessors:**

- `win32-error-code` (condition): Returns the numeric error code.
  
- `win32-error-symbol` (condition): Returns the keyword symbol for the error.
  
- `win32-error-description` (condition): Returns the descriptive string for the error.
  

**Example of handling an error:**

``` lisp
(handler-case
    ;; For example, signal an error after a failed API call
    (error 'cl-win32-errors:win32-error :code 87)
  (cl-win32-errors:win32-error (e)
    (format t "Caught a Win32 Error!~%")
    (format t "  Code: ~A~%" (cl-win32-errors:win32-error-code e))
    (format t "  Symbol: ~A~%" (cl-win32-errors:win32-error-symbol e))
    (format t "  Description: ~A~%" (cl-win32-errors:win32-error-description e))))
```

## Running Tests

To run the test suite from the command line, use `qlot exec`:

``` powershell
qlot exec ros run --eval "(asdf:test-system :cl-win32-errors/tests)" --quit
```

Alternatively, from within a REPL started with `qlot exec ros run`:

``` lisp
(asdf:test-system :cl-win32-errors/tests)
```

## Contributing

Bug reports and pull requests are welcome on GitHub. Please ensure the test suite passes before submitting a pull request.

## License

This project is licensed under the MIT License.