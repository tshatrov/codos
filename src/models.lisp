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
:get-full-user-data
:get-document-data
:get-all-viewsets
:create-viewfield))

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

(defun group-plists (plists id-key group-key filter-key count)
  (loop with hash = (make-hash-table)
       for plist in plists
       for id = (getf plist id-key)
       for grouped = (gethash id hash)
       for vars = (nthcdr count plist)
       if grouped
       do (push (getf grouped group-key) vars)
       else
       do (setf (gethash id hash)
                (list*
                 group-key (when (getf vars filter-key) (list vars))
                 (subseq plist 0 count)))
       and collect id into keys
       finally
       (return (loop for idk in keys
                    for plist = (gethash idk hash)
                    do (setf (getf plist group-key) (reverse (getf plist group-key)))
                    collect plist))))

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

;; viewsets

(defun get-all-viewsets ()
  (with-connection (db)
    (let ((viewsets
           (retrieve-all
            (select '(:viewset.id :viewset.title :vv.order :viewfield.*)
              (from :viewset)
              (left-join (:as :viewset-view :vv) :on (:= :vv.viewset :viewset.id))
              (left-join :viewfield :on (:= :vv.view :viewfield.id))
              (order-by :viewset.id :vv.order)))))
      (group-plists viewsets :id :views :id 4))))

(defun create-viewfield (abbr desc)
  (with-connection (db)
    (let ((old-vf (retrieve-one (select :id (from :viewfield) (where (:= :abbr abbr))))))
      (if old-vf (error 'field-error
                        :field :abbr
                        :message "There already exists a viewfield with this abbreviation")
          (execute
           (insert-into :viewfield
             (set= :abbr abbr :description desc)))))))

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

;; document

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

(defun get-document-data (document)
  (setf (getf document :content) (get-document-content document))
  document)

(defun get-document-content (document)
  "")

;; section

(defun create-section ()
  )
