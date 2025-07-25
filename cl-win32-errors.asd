;;;; cl-win32-errors.asd

(defsystem #:cl-win32-errors
  :description "A library for translating Windows API error codes."
  :author "Andrew D. France <andrewforlua@gmail.com>"
  :license "MIT"
  :version "1.0.0"
  :depends-on ()
  :components ((:file "package")
               (:file "cl-win32-errors" :depends-on ("package"))
               ;; Load all the error code files from the src/ directory.
               (:module "src"
                :depends-on ("cl-win32-errors")
                :components ((:file "core")
                             (:file "network")
                             (:file "rpc")
                             (:file "security")
                             (:file "extended")
                             (:file "cluster")
                             (:file "transactional")
                             (:file "ds")
                             (:file "dns")
                             (:file "winsock")))))

(defsystem #:cl-win32-errors/tests
  :description "Test suite for cl-win32-errors."
  :author "Your Name <you@example.com>"
  :license "MIT"
  :depends-on (:cl-win32-errors
               :fiveam)
  :components ((:module "tests"
                :components ((:file "main"))))
  :perform (test-op (op c)
             (uiop:symbol-call :fiveam '#:run!
                               (uiop:find-symbol* '#:cl-win32-errors-suite
                                                  '#:cl-win32-errors/tests))))
