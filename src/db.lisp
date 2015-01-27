(in-package :cl-user)
(defpackage codos.db
  (:use :cl :sxql :datafly)
  (:import-from :codos.config
                :config)
  (:export :connection-settings
           :db
           :with-connection
           :init-db
           :returning))
(in-package :codos.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defmacro execute-many (&body forms)
  `(progn ,@(loop for form in forms collect `(execute ,form))))


(defstruct (returning-clause
             (:include sxql.sql-type:statement-clause (name "RETURNING"))
             (:constructor make-returning-clause
                           (&rest fields
                                  &aux (statement (apply #'sxql.sql-type:make-sql-splicing-list fields))))))

(defmethod make-clause ((clause-name (eql :returning)) &rest args)
  (apply #'make-returning-clause (mapcar #'sxql.operator:detect-and-convert args)))

(defun returning (&rest fields)
  (apply #'make-clause :returning fields))

(defparameter *tables*
  '(:user :document :hub :hub-doc
    :viewfield :viewset :viewset-view
    :section :presentation
    :chunk :chunk-view
    ))

(defun drop-tables ()
  (with-connection (db)
    (loop for table in (reverse *tables*)
       do (execute (drop-table table :if-exists t)))))

(defun init-tables ()
  (with-connection (db)
    (execute-many
      (create-table :user
          ((id :type 'serial :primary-key t)
           (login :type 'text :unique t)
           (fullname :type 'text)
           (email :type 'text)
           (adminp :type 'boolean)
           (hash :type 'text)
           (registered :type 'timestamp)
           ))

      (create-table :document
          ((id :type 'serial :primary-key t)
           (title :type 'text)
           (author :type 'integer)
           (slug :type 'text :unique t)
           (created :type 'timestamp)
           (modified :type 'timestamp)
           )
        (foreign-key '(:author) :references '(:user :id)))
      
      (create-table :hub
          ((id :type 'serial :primary-key t)
           (title :type 'text)
           (author :type 'integer)
           (slug :type 'text :unique t)
           )
        (foreign-key '(:author) :references '(:user :id)))

      (create-table :hub-doc
          ((order :type 'integer)
           (hub :type 'integer)
           (document :type 'integer))
        (foreign-key '(:hub) :references '(:hub :id))
        (foreign-key '(:document) :references '(:document :id))
        (primary-key '(:hub :document)))
      
      (create-table :viewfield
          ((id :type 'serial :primary-key t)
           (abbr :type 'text)
           (description :type 'text)
           ))

      ;; TODO: add viewactions, such as a link to some website

      (create-table :viewset
          ((id :type 'serial :primary-key t)
           (title :type 'text)
           ))

      (create-table :viewset-view
          ((order :type 'integer)
           (viewset :type 'integer)
           (view :type 'integer))
        (foreign-key '(:viewset) :references '(:viewset :id))
        (foreign-key '(:view) :references '(:viewfield :id))
        (primary-key '(:viewset :view)))
      
      (create-table :section
          ((id :type 'serial :primary-key t)
           (level :type 'integer)
           (order :type 'integer)
           (document :type 'integer)
           (default-viewset :type 'integer))
        (foreign-key '(:document) :references '(:document :id))
        (foreign-key '(:default-viewset) :references '(:viewset :id)))

      (create-table :presentation
          ((id :type 'serial :primary-key t)
           (section :type 'integer)
           (order :type 'integer)
           (viewset :type 'integer)
           (type :type 'text))
        (foreign-key '(:section) :references '(:section :id))
        (foreign-key '(:viewset) :references '(:viewset :id)))

      (create-table :chunk
          ((id :type 'serial :primary-key t)
           (presentation :type 'integer)
           (order :type 'integer))
        (foreign-key '(:presentation) :references '(:presentation :id)))

      (create-table :chunk-view
          ((id :type 'serial :primary-key t)
           (chunk :type 'integer)
           (view :type 'integer)
           (author :type 'integer)
           (timestamp :type 'timestamp)
           (selected :type 'boolean)
           (text :type 'text))
        (foreign-key '(:author) :references '(:user :id))
        (foreign-key '(:chunk) :references '(:chunk :id))
        (foreign-key '(:view) :references '(:viewfield :id)))
      )))

(defun create-indexes ()
  (with-connection (db)
    (execute-many
      (create-index "document_author_idx" :on '(:document :author))
      (create-index "section_document_idx" :on '(:section :document :order))
      (create-index "presentation_section_idx" :on '(:presentation :section :order))
      (create-index "chunk_presentation_idx" :on '(:chunk :presentation :order))
      (create-index "chunk_view_author_idx" :on '(:chunk-view :author))
      (create-index "chunk_view_timestamp_idx" :on '(:chunk-view :timestamp))
      (create-index "chunk_view_idx" :on '(:chunk-view :chunk :view))
      (create-index "chunk_view_selected_idx" :on '(:chunk-view :selected)))))
      
(defun init-db ()
  (with-connection (db)
    (drop-tables)
    (init-tables)
    (create-indexes)))


(defun init-fixtures ()
  (with-connection (db)
    ;; default viewset
    (let ((default-view
           (retrieve-one
            (insert-into :viewfield
              (set= :abbr "def"
                    :description "default")
              (returning :id))))
          (default-viewset
           (retrieve-one
            (insert-into :viewset
              (set= :id 1 :title "Default")
              (returning :id)))))
      (execute
       (insert-into :viewset-view
         (set= :order 0
               :viewset (getf default-viewset :id)
               :view (getf default-view :id)))))))
      
      
             
      
