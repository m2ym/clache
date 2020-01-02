(in-package :cl-user)

(defpackage clache-asd
  (:use :cl :asdf))
(in-package :clache-asd)

(defsystem :clache
  :version (:read-from-file "version.lisp-expr")
  :author "Tomohiro Matsuyama"
  :maintainer "Olexiy Zamkoviy"
  :license "LLGPL"
  :depends-on (:alexandria
               :trivial-garbage
               :babel
               :ironclad
               :cl-fad
               :cl-store
               :cl-annot
               :cl-syntax
               :cl-syntax-annot)
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "protocol")
                             (:module "stores"
                              :serial t
                              :components ((:file "memory")
                                           (:file "file")))
                             (:file "api")))))
