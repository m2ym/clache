(in-package :cl-user)

(defpackage cl-cache-asd
  (:use :cl :asdf))
(in-package :cl-cache-asd)

(defsystem cl-cache
  :version "0.1"
  :author "Tomohiro Matsuyama"
  :license "LLGPL"
  :depends-on (:alexandria
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
                             (:module "storages"
                              :serial t
                              :components ((:file "memory")
                                           (:file "file")))
                             (:file "api")))))
