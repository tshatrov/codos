(in-package :cl-user)
(defpackage codos.crypto
  (:use :cl)
  (:export
   :generate-csrf-token))

(in-package :codos.crypto)

(setf ironclad:*prng* (ironclad:make-prng :fortuna))
(ironclad:read-os-random-seed :random)

(defun generate-csrf-token (&optional (bytes 24))
  (cl-base32:bytes-to-base32
   (ironclad:random-data bytes)))
