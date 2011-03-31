(in-package :cl-cache)
(annot:enable-annot-syntax)

(defun md5-hex-string (object)
  (let* ((key-string (prin1-to-string object))
         (octets (babel:string-to-octets key-string))
         (digest (ironclad:digest-sequence 'ironclad:md5 octets))
         (hex-string (format nil "~{~2,'0X~}" (coerce digest 'list))))
    (string-downcase hex-string)))
