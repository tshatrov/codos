(in-package :cl-user)
(defpackage codos.models
  (:use :cl :sxql :codos.db :datafly)
  (:import-from :codos.config
                :config)
  (:import-from :codos.forms
                :field-error)
  )

(in-package :codos.models)

(defmodel (user
           (:inflate registered #'datetime-to-timestamp))
  id
  login
  fullname
  email
  adminp
  session
  registered)

(defun get-plist (model)
  (loop for slot in (closer-mop:class-slots (class-of model))
     for slot-name = (closer-mop:slot-definition-name slot)
     nconc (list (alexandria:make-keyword slot-name)
                 (slot-value model slot-name))))

(defun create-user (login password &key (fullname "") (email ""))
  (with-connection (db)
    (when (retrieve-one
           (select :id
             (from :user)
             (where (:= :login login))))
      (error 'field-error
             :field :user
             :message "Username already exists"))
    (execute
     (insert-into :user
       (set= :login login
             :fullname fullname
             :email email
             :adminp nil
             :registered '(:raw "NOW()")
             :hash (generate-password-hash login password)
             )))))
