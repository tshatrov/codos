(in-package :cl-user)
(defpackage codos.crypto
  (:use :cl)
  (:export
   :generate-csrf-token
   :generate-password-hash
   :check-password))

(in-package :codos.crypto)

(setf ironclad:*prng* (ironclad:make-prng :fortuna))
(ironclad:read-os-random-seed :random)

(defun generate-csrf-token (&optional (bytes 24))
  (cl-base32:bytes-to-base32
   (ironclad:random-data bytes)))

(defun string-to-octets (string)
  (coerce 
   (flexi-streams:string-to-octets string :external-format :utf8)
   '(vector (unsigned-byte 8))))

(defun generate-password-hash (login password)
  (declare (ignorable login))
  (ironclad:pbkdf2-hash-password-to-combined-string
   (string-to-octets password)))

(defun check-password (login password hash)
  (declare (ignorable login))
  (ironclad:pbkdf2-check-password
   (string-to-octets password)
   hash))
