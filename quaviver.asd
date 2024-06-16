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
                             (:file "interface")
                             (:file "float-integer-2")
                             (:file "digits-integer")
                             (:file "integer-digits")))))

(defsystem "quaviver/math"
  :description "Math routines for Quaviver"
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
                :pathname "code/math/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

(defsystem "quaviver/ieee754"
  :description "IEEE-754 float conversions"
  ;; Contains code from Nibbles.
  :license "MIT AND BSD-3-Clause"
  :author ("Paul A. Patience")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver")
  :components ((:module "code"
                :pathname "code/ieee754/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

(defsystem "quaviver/trailing-zeros"
  :description "Trailing zero removal"
  :license "MIT AND (Apache-2.0 WITH LLVM-exception OR BSL-1.0)"
  :author ("Tarn W. Burton"
           "Paul A. Patience")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver"
               (:feature :sbcl "sb-rotate-byte"))
  :components ((:module "code"
                :pathname "code/trailing-zeros/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

(defsystem "quaviver/native"
  :description "Current implementation's native algorithms"
  :license "MIT"
  :author ("Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver")
  :components ((:module "code"
                :pathname "code/native/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

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

(asdf:defsystem "quaviver/burger-dybvig/unit-test"
  :description "Unit testing suite for Quaviver/Burger Dybvig algorithm."
  :author "Tarn W. Burton"
  :license "MIT"
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver/burger-dybvig"
               "parachute")
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :parachute :test :quaviver/burger-dybvig/unit-test))
  :components ((:module code
                :pathname "code/burger-dybvig/unit-test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")))))

(defsystem "quaviver/schubfach"
  :description "Shubfach algorithm for Quaviver"
  :license "MIT"
  :author ("Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver"
               "quaviver/math"
               "quaviver/trailing-zeros")
  :components ((:module "code"
                :pathname "code/schubfach/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

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
               "quaviver/burger-dybvig"
               "quaviver/schubfach")
  :perform (test-op (op c)
             (symbol-call :quaviver/ansi-test :test))
  :components ((:module "code"
                :pathname "code/ansi-test/"
                :serial t
                :components ((:file "packages")
                             (:file "test")
                             (:static-file "expected-failures.sexp")))))

(defsystem "quaviver/benchmark"
  :description "Benchmark system for Quaviver"
  :license "MIT"
  :author ("Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("the-cost-of-nothing"
               "cl-spark"
               "cl-ascii-table"
               "quaviver/burger-dybvig"
               "quaviver/native"
               "quaviver/schubfach")
  :components ((:module "code"
                :pathname "code/benchmark/"
                :serial t
                :components ((:file "packages")
                             (:file "float-decimal")))))
