(cl:in-package #:asdf-user)

(defsystem "quaviver"
  :description "A portable and extensible floating point string library"
  :license "MIT"
  :author ("Robert Strandh"
           "Paul A. Patience"
           "Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ()
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:file "interface")))))

(defsystem "quaviver/burger-dybvig"
  :description "Burger Dybvig algorithm for Quaviver"
  :license "MIT"
  :author ("Robert Strandh"
           "Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver")
  :components ((:module "code"
                :pathname "code/burger-dybvig/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

(defsystem "quaviver/float-bits"
  :description "Floats to bits conversions"
  ;; Contains code from Nibbles.
  :license "MIT AND BSD-3-Clause"
  :author ("Paul A. Patience")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ()
  :components ((:module "code"
                :components ((:file "float-bits")))))

(defsystem "quaviver/ansi-test"
  :description "ANSI Test system for Quaviver"
  :license "MIT"
  :author ("Tarn W. Burton"
           "Robert Strandh")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("ansi-test-harness"
               "invistra-extrinsic"
               "quaviver/burger-dybvig")
  :perform (test-op (op c)
             (symbol-call :quaviver/ansi-test :test))
  :components ((:module "code"
                :pathname "code/ansi-test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")
                             (:static-file "expected-failures.sexp")))))
