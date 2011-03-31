(in-package :cl-cache)
(annot:enable-annot-syntax)

@export
(defclass storage () ())

(deftype expire ()
  '(or null integer))

@export
(defgeneric cache-exists-p (key storage))

@export
(defgeneric load-cache (key storage))

@export
(defgeneric store-cache (key value expire storage))

@export
(defgeneric delete-cache (key storage))

@export
(defgeneric clear-cache (storage))

@export
(defun check-storage (storage)
  (check-type storage storage))

@export
(defun check-expire (expire)
  (check-type expire expire))

@export
(defun never-expire-p (expire)
  (eq expire nil))
