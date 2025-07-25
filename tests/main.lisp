;;;; tests/main.lisp
;;;
;;; This file defines the test package, the main test suite, and all tests.

(defpackage #:cl-win32-errors/tests
  (:use #:cl #:cl-win32-errors #:fiveam)
  (:export #:cl-win32-errors-suite
           #:run-tests))

(in-package #:cl-win32-errors/tests)

(def-suite cl-win32-errors-suite
  :description "Parent test suite for the cl-win32-errors library.")

(in-suite cl-win32-errors-suite)

(defun run-tests ()
  "Run all cl-win32-errors tests."
  (run! 'cl-win32-errors-suite))

;;; ----------------------------------------------------------------------------
;;; Tests
;;; ----------------------------------------------------------------------------

(test get-error-details-tests
  "Tests the `get-error-details` function."
  (let ((details (get-error-details 5)))
    (is-true details "Details should not be nil for a known error.")
    (is (eq :ERROR_ACCESS_DENIED (getf details :symbol)))
    (is (string= "0x5" (getf details :hex))))
  (is-false (get-error-details 99999) "Details should be nil for an unknown error."))

(test format-error-message-tests
  "Tests the `format-error-message` function and its verbosity levels."
  (let ((message (format-error-message 2)))
    (is (string= "Win32 Error 2: The system cannot find the file specified." message)))
  (let ((message (format-error-message 2 :verbosity :verbose)))
    (is (string= "Win32 Error 2 (0x2/ERROR_FILE_NOT_FOUND): The system cannot find the file specified." message))))

(test win32-error-condition-tests
  "Tests the custom `win32-error` condition."
  (let ((err (make-condition 'win32-error :code 5 :details (get-error-details 5))))
    (is (eq :ERROR_ACCESS_DENIED (win32-error-symbol err)))
    (is (string= "Access is denied." (win32-error-description err)))))

(test all-registered-errors-are-valid
  "Performs a sanity check on every single registered error code."
  (let ((error-count 0))
    (maphash (lambda (code details)
               (incf error-count)
               (is (numberp code))
               (is (keywordp (getf details :symbol)))
               (is (stringp (getf details :description))))
             cl-win32-errors::*win32-error-map*)
    ;; A sanity check to make sure we actually tested something.
    (is (> error-count 2300) "The error map should contain over 2300 entries.")))
