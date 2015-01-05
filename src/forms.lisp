(in-package :cl-user)
(defpackage codos.forms
  (:use :cl :codos.view)
  (:import-from :codos.config
                :config)
  (:export
   :field-error
   :field-error-field
   :field-error-message))

(in-package :codos.forms)

(define-condition field-error (error)
  ((field :initarg :field
          :reader field-error-field)
   (message :initarg :message
            :reader field-error-message))
  (:report (lambda (condition stream)
             (format stream "Error in field ~a: ~a"
                     (field-error-field condition)
                     (field-error-message condition)))))
