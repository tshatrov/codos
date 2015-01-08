(in-package :cl-user)
(defpackage codos.forms
  (:use :cl :codos.view :1forms :codos.models)
  (:import-from :codos.config
                :config)
  (:import-from :caveman2
                :*response*
                :*session*)
  (:export
   :register-form
   :login-form))

(in-package :codos.forms)

(defun validate-login (str)
  (let ((str (string-downcase str)))
    (validate-length str 3 20)
    (validate-regex str "[a-z][a-z0-9_-]*" "only alphanumeric characters, _ and - are allowed; must start with letter")
    ))

(defun validate-password (str)
  (validate-length str 6 255)
  )

(defun validate-email (str)
  (validate-length str 3 255 t)
  (unless (or (alexandria:emptyp str) (find #\@ str))
    (error 'field-error :message "Invalid email address")))

(def-form register-form ()
  (:login string-field
          :validator 'validate-login
          :name "login"
          :label "Username *"
          )
  (:password password-field
             :validator 'validate-password
             :name "pw"
             :label "Password *")
  (:fullname string-field
              :validator (lambda (str)
                           (validate-length str 3 255 t))
              :name "fullname"
              :label "Full name")
  (:email string-field
          :validator 'validate-email
          :name "email"
          :label "Email"))

(defmethod validate :after ((form register-form))
  (let ((data (form-data form)))
    (create-user
     (getf data :login)
     (getf data :password)
     :fullname (getf data :fullname "")
     :email (getf data :email ""))))


(def-form login-form ()
  (:login string-field
          :validator (lambda (str) (validate-length str nil 20))
          :name "login"
          :label "Username"
          )
  (:password password-field
             :validator (lambda (str) (validate-length str nil 255))
             :name "pw"
             :label "Password"))

(defmethod validate :after ((form login-form))
  (let ((data (form-data form)))
    (login-user (getf data :login) (getf data :password))))
  
