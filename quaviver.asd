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
  :if-feature (:or :abcl :allegro :ccl :clasp :clisp :cmucl :ecl :lispworks
                   :mezzano :sbcl)
  :depends-on ("alexandria"
               "trivial-features")
  :components ((:module "code"
                :serial t
                :components ((:file "packages")
                             (:module "condition"
                              :serial t
                              :components ((:file "utility")))
                             (:file "interface")
                             (:file "traits")
                             (:module "math"
                              :serial t
                              :components ((:file "log-expt")
                                           (:file "utility")
                                           (:file "count-digits")
                                           (:file "implementation")
                                           (:file "expt")
                                           (:file "round-to-odd")))
                             (:file "bits-float-form")
                             (:file "bits-float")
                             (:file "float-bits-form")
                             (:file "float-bits")
                             (:file "internal-integer-float-form")
                             (:file "integer-float")
                             (:file "float-internal-integer-form")
                             (:file "float-integer")
                             (:file "parse-digits")
                             (:file "compose-digits")
                             (:file "read-digits")
                             (:file "write-digits")
                             (:file "number-parser")))))

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
                             (:file "float-integer")
                             (:file "integer-float")))))

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
               "quaviver/trailing-zeros")
  :components ((:module "code"
                :pathname "code/schubfach/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

(defsystem "quaviver/dragonbox"
  :description "Dragonbox algorithm for Quaviver"
  :license "MIT AND (Apache-2.0 WITH LLVM-exception OR BSL-1.0)"
  :author ("Paul A. Patience")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver"
               "quaviver/trailing-zeros")
  :components ((:module "code"
                :pathname "code/dragonbox/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

(defsystem "quaviver/jaffer"
  :description "Jaffer algorithm for Quaviver"
  :license "MIT"
  :author ("Alex Wood"
           "Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver")
  :components ((:module "code"
                :pathname "code/jaffer/"
                :serial t
                :components ((:file "packages")
                             (:file "implementation")))))

(defsystem "quaviver/liebler"
  :description "Liebler algorithm for Quaviver"
  :license "MIT"
  :author ("Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver")
  :components ((:module "code"
                :pathname "code/liebler/"
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
               "quaviver/schubfach"
               "quaviver/dragonbox")
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
               "quaviver/native"
               "quaviver/burger-dybvig"
               "quaviver/schubfach"
               "quaviver/dragonbox"
               "quaviver/jaffer"
               "quaviver/liebler")
  :components ((:module "code"
                :pathname "code/benchmark/"
                :serial t
                :components ((:file "packages")
                             (:file "results")
                             (:file "report")
                             (:file "float-integer")
                             (:file "integer-float")))))

(defsystem "quaviver/compare"
  :description "Compare implementations of Quaviver protocol"
  :license "MIT"
  :author ("Paul A. Patience"
           "Tarn W. Burton")
  :version (:read-file-form "version.sexp")
  :homepage "https://github.com/s-expressionists/Quaviver"
  :bug-tracker "https://github.com/s-expressionists/Quaviver/issues"
  :source-control (:git "https://github.com/s-expressionists/Quaviver.git")
  :depends-on ("quaviver/burger-dybvig"
               "quaviver/schubfach"
               "quaviver/dragonbox"
               "quaviver/jaffer"
               "quaviver/liebler"
               "lparallel")
  :components ((:module "code"
                :pathname "code/compare/"
                :serial t
                :components ((:file "packages")
                             (:file "utility")
                             (:file "interval")
                             (:file "test")
                             (:file "float-integer")
                             (:file "integer-float")))))
