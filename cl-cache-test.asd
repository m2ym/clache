(defpackage cl-cache-test-asd
  (:use :cl :asdf))
(in-package :cl-cache-test-asd)

(defsystem cl-cache-test
  :depends-on (:cl-cache
               :cl-test-more)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "api")))))
