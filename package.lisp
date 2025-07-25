;;;; package.lisp

(defpackage #:cl-win32-errors
  (:use #:cl)
  (:documentation "Provides utilities for handling Windows API error codes.")
  (:export
   ;; Core functions
   #:get-error-details
   #:format-error-message

   ;; Condition for signaling Windows errors
   #:win32-error

   ;; Accessors for the condition
   #:win32-error-code
   #:win32-error-symbol
   #:win32-error-description))
