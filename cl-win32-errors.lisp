;;;; cl-win32-errors.lisp

(in-package #:cl-win32-errors)

;;; ----------------------------------------------------------------------------
;;; Error Code Database and API
;;; ----------------------------------------------------------------------------

(defvar *win32-error-map* (make-hash-table)
  "A hash table mapping numeric error codes to their details.")

(defun register-error (code symbol description)
  "Registers a Win32 error code and its details in the database."
  (setf (gethash code *win32-error-map*)
        `(:code ,code
          :hex ,(format nil "0x~X" code)
          :symbol ,symbol
          :description ,description)))

(defun get-error-details (error-code)
  "Looks up a Win32 error code and returns a plist of its details.
   Returns NIL if the code is not found."
  (gethash error-code *win32-error-map*))

(defun format-error-message (error-code &key (verbosity :default))
  "Formats a Win32 error code into a human-readable string.
   VERBOSITY can be :DEFAULT or :VERBOSE."
  (let ((details (get-error-details error-code)))
    (if details
        (case verbosity
          (:verbose (format nil "Win32 Error ~A (~A/~A): ~A"
                            error-code
                            (getf details :hex)
                            (getf details :symbol)
                            (getf details :description)))
          (t (format nil "Win32 Error ~A: ~A"
                     error-code
                     (getf details :description))))
        (format nil "Unknown Win32 Error Code: ~A" error-code))))

;;; ----------------------------------------------------------------------------
;;; Condition System Integration
;;; ----------------------------------------------------------------------------

(define-condition win32-error (error)
  ((code :initarg :code :reader win32-error-code)
   (details :initarg :details :reader win32-error-details))
  (:report (lambda (condition stream)
             ;; The condition report will use the verbose format by default.
             (format stream "A Windows API call failed: ~A"
                     (format-error-message (win32-error-code condition) :verbosity :verbose)))))

(defun win32-error-symbol (condition)
  "Returns the symbolic name of the error, e.g., :ERROR_ACCESS_DENIED."
  (getf (win32-error-details condition) :symbol))

(defun win32-error-description (condition)
  "Returns the descriptive string for the error."
  (getf (win32-error-details condition) :description))