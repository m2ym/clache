(defpackage clache-test-asd
  (:use :cl :asdf))
(in-package :clache-test-asd)

(defsystem clache-test
  :depends-on (:clache
               :cl-test-more)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "api")))))
