(in-package :cl-cache)
(annot:enable-annot-syntax)

(defun symbol-fqn (symbol)
  (check-type symbol symbol)
  (format nil "~A:~A"
          (package-name (symbol-package symbol))
          (symbol-name symbol)))

(defun object-to-string (object)
  (etypecase object
    (symbol (symbol-fqn object))
    (t (princ-to-string object))))

(defun md5-hex-string (object)
  (let* ((key-string (object-to-string object))
         (octets (babel:string-to-octets key-string))
         (digest (ironclad:digest-sequence 'ironclad:md5 octets))
         (hex-string (format nil "~{~2,'0X~}" (coerce digest 'list))))
    (string-downcase hex-string)))
