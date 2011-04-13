(in-package :cl-cache)
(use-syntax annot-syntax)

(defun symbol-fqn (symbol)
  "Return a fully qualified name of SYMBOL in string. For
example, (symbol-fqn 'if) will return \"COMMON-LISP:IF\"."
  (check-type symbol symbol)
  (format nil "~A:~A"
          (package-name (symbol-package symbol))
          (symbol-name symbol)))

(defun object-to-string (object)
  "Convert OBJECT into string by using PRINC-TO-STRING if OBJECT is
not a symbol, or by using SYMBOL-FQN if OBJECT is a symbol."
  (etypecase object
    (symbol (symbol-fqn object))
    (t (princ-to-string object))))

(defun md5-hex-string (string)
  "Return a MD5 digest of STRING in hex string."
  (let* ((octets (babel:string-to-octets string))
         (digest (ironclad:digest-sequence 'ironclad:md5 octets))
         (hex-string (format nil "~{~2,'0X~}" (coerce digest 'list))))
    (string-downcase hex-string)))
