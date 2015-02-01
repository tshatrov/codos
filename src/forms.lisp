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
   :login-form
   :doc-settings-form
   :viewfield-form))

(in-package :codos.forms)

(defclass add-edit-form (form) ())

(defgeneric finalize-add (form)
  (:method (form)))

(defgeneric finalize-edit (form)
  (:method (form)))

(defun validate-csrf (str)
  (unless (equal str (get-csrf-token))
    (error 'field-error :message "CSRF check failed")))

(def-form csrf-form (form)
  (:csrf hidden-field :name "csrf" :validator 'validate-csrf :initial (get-csrf-token)))

(defmethod finalize ((form add-edit-form))
  (if (getf (form-vars form) :id)
      (finalize-edit form)
      (finalize-add form)))

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

(def-form register-form (csrf-form)
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

(defmethod finalize ((form register-form))
  (let ((data (form-data form)))
    (create-user
     (getf data :login)
     (getf data :password)
     :fullname (getf data :fullname "")
     :email (getf data :email ""))))


(def-form login-form (csrf-form)
  (:login string-field
          :validator (lambda (str) (validate-length str nil 20))
          :name "login"
          :label "Username"
          )
  (:password password-field
             :validator (lambda (str) (validate-length str nil 255))
             :name "pw"
             :label "Password"))

(defmethod finalize ((form login-form))
  (let ((data (form-data form)))
    (login-user (getf data :login) (getf data :password))))

(def-form doc-settings-form (csrf-form)
  (:title string-field
          :validator (lambda (str) (validate-length str 1 512))
          :name "title"
          :label "Title"))
  
(defmethod finalize ((form doc-settings-form))
  (let ((user (get-user-info))
        (data (form-data form)))
    (create-document
     (getf data :title)
     (getf user :id))))
     
(def-form viewfield-form (add-edit-form csrf-form)
  (:abbr string-field
         :validator (lambda (str) (validate-length str 1 10))
         :name "abbr"
         :label "Abbreviation")
  (:desc string-field
         :validator (lambda (str) (validate-length str 1 255))
         :name "desc"
         :label "Description"))

(defmethod finalize-add ((form viewfield-form))
  (let ((data (form-data form)))
    (create-viewfield (getf data :abbr) (getf data :desc))))
   
