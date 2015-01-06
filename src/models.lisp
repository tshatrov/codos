(in-package :cl-user)
(defpackage codos.models
  (:use :cl :sxql :codos.db :codos.crypto :datafly)
  (:import-from :caveman2
                :*session*)
  (:import-from :codos.config
                :config)
  (:import-from :iforms
                :field-error)
  (:import-from :cl-annot.class
                :export-constructors)
  (:export
   :create-user
   :get-plist
   ;; :user
   ;; :user-id
   ;; :user-login
   ;; :user-fullname
   ;; :user-email
   ;; :user-adminp
   ;; :user-registered
:login-user
:logout-user))

(in-package :codos.models)

;; (defmodel (user
;;            (:inflate registered #'datetime-to-timestamp))
;;   id
;;   login
;;   fullname
;;   email
;;   adminp
;;   registered)

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

(defun login-user (login password)
  (with-connection (db)
    (let* ((login (string-downcase login))
           (user (retrieve-one
                  (select :*
                    (from :user)
                    (where (:= :login login)))
                  )))
      (unless (and user (check-password login password (getf user :hash)))
        (error 'field-error
               :field nil
               :message "Invalid username and/or password"))
      (setf (getf user :hash) nil)
      (setf (gethash :user *session*) user))))

(defun logout-user ()
  (setf (gethash :user *session*) nil))
