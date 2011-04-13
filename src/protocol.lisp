(in-package :cl-cache)
(use-syntax annot-syntax)

#|

Protocol
--------

TODO

|#

@export
(defclass storage ()
     ()
  (:documentation "An abstract class of storages. All storages must
inherit from this class."))

(deftype expire ()
  "Type for expiration time."
  '(or null integer))

@export
(defgeneric load-cache (key storage)
  (:documentation "Try to retrieve a cache indicated by KEY from
STORAGE and return values of the cache value and a boolean whether the
cache exists in STORAGE. The cache value should be NIL if such the
cache doesn't exist or has been expired."))

@export
(defgeneric store-cache (key value expire storage)
  (:documentation "Store a cache VALUE with KEY into STORAGE. EXPIRE
is a keep time in seconds. If EXPIRE is NIL, the cache will never
expired. This function should return the value that has been
stored."))

@export
(defgeneric delete-cache (key storage)
  (:documentation "Remove a cache indicated by KEY from STORAGE. If
the cache has been successfully removed, this function should return
T, otherwise should return NIL."))

@export
(defgeneric clear-cache (storage)
  (:documentation "Remove all caches from STORAGE. Any object can be
returned."))

@export
(defgeneric cache-key-to-string (key)
  (:documentation "This function converts any type of KEY into
string. This should be an injective function, meaning this should not
lose the information about key.")
  (:method (key) (object-to-string key)))

@export
(defun check-storage (storage)
  "Assert that type of STORAGE is STORAGE class."
  (check-type storage storage))

@export
(defun check-expire (expire)
  "Assert that type of EXPIRE is EXPIRE type."
  (check-type expire expire))

@export
(defun never-expire-p (expire)
  "Return T if EXPIRE represents caches will never be expired."
  (eq expire nil))
