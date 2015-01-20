(in-package :cl-user)
(defpackage codos.models
  (:use :cl :sxql :codos.db :codos.crypto :datafly)
  (:import-from :caveman2
                :*session*)
  (:import-from :codos.config
                :config)
  (:import-from :1forms
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
:logout-user
:create-hub
:list-table
:db-let
:create-document
:get-full-user-data))

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

(defmacro list-table (table &body clauses)
  `(with-connection (db)
     (retrieve-all
      (select :*
        (from ,table)
        ,@clauses))))

(defmacro db-let ((var table &rest clauses) on-fail &body body)
  `(with-connection (db)
     (let ((,var (retrieve-one
                  (select :*
                    (from ,table)
                    ,@clauses))))
       (cond (,var ,@body)
             (t ,on-fail)))))

;; user

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
      (setf (getf user :registered) (datetime-to-timestamp (getf user :registered)))
      (setf (gethash :user *session*) user))))

(defun logout-user ()
  (setf (gethash :user *session*) nil))

(defun get-full-user-data (user)
  (setf (getf user :documents)
        (list-table :document
          (where (:= :author (getf user :id)))
          (order-by (:desc :modified))))
  user)

;; hub

(defun generate-slug (str &key table)
  (setf str (ppcre:regex-replace-all "[$&+,/:;=?@\\s\"<>#%{}|\\\\^~\\[\\]`]" str "-"))
  (setf str (ppcre:regex-replace-all "-+" str "-"))
  (setf str (string-downcase str))
  (when (> (length str) 20)
    (setf str (subseq str 0 20)))
  (when table
    (let* ((existing (retrieve-all
                      (select :slug (from table)
                              (where (:like :slug (format nil "~a%" str))))))
           (slugs (mapcar 'second existing)))
      (when (member str slugs :test 'equal)
        (loop for n from 1
             for newstr = (format nil "~a~a" str n)
             while (member newstr slugs)
             finally (setf str newstr)))))
  str)

(defun create-hub (title author-id &optional slug)
  (with-connection (db)
    (execute
     (insert-into :hub
       (set= :title title
             :author author-id
             :slug (or slug (generate-slug title :table :hub))
             )))))

(defun create-document (title author-id &optional slug)
  (with-connection (db)
    (execute
     (insert-into :document
       (set= :title title
             :author author-id
             :slug (or slug (generate-slug title :table :document))
             :created '(:raw "NOW()")
             :modified '(:raw "NOW()")
             )))))
