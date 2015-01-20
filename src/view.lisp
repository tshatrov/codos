(in-package :cl-user)
(defpackage codos.view
  (:use :cl)
  (:import-from :codos.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :*session*)
  (:import-from :clack.response
                :headers)
  (:import-from :cl-emb
                :*escape-type*
                :*case-sensitivity*
                :*function-package*
                :execute-emb)
  (:import-from :datafly
                :encode-json)
  (:import-from :codos.crypto
                :generate-csrf-token)
  (:export :*default-layout-path*
           :*default-layout-env*
           :render
           :render-json
           :with-layout
           :get-global-context
           :set-asset-version
           :get-asset-version
           :get-user-info))
(in-package :codos.view)

(defvar *default-layout-directory* #P"layouts/")
(defvar *default-layout-path* #P"default.tmpl")

(defvar *default-layout-env* '())

(defun path (filename)
  (asdf:system-relative-pathname :codos filename))

(defparameter *asset-version-file* (path "ver"))

(defparameter *asset-version*
  (with-open-file (s *asset-version-file*)
    (read-line s)))

(defun set-asset-version ()
  (let ((ver (format nil "~a" (get-universal-time))))
    (with-open-file (s *asset-version-file* :direction :output :if-exists :supersede)
      (write-string ver s))
    (setf *asset-version* ver)))

(defun get-asset-version (&key update)
  (if update 
      (setf *asset-version*
            (with-open-file (s *asset-version-file*)
              (read-line s)))
      *asset-version*))

(defun get-csrf-token ()
  (or
   (gethash :csrf-token *session*)
   (setf (gethash :csrf-token *session*)
         (generate-csrf-token))))

(defun get-user-info ()
  (gethash :user *session*))

(defparameter *global-context* nil)

(defun get-global-context ()
  (list* :ver (get-asset-version)
         :csrf-token (get-csrf-token)
         :user (get-user-info)
         *default-layout-env*))

(defun render (template-path &optional env)
  (let ((emb:*escape-type* :html)
        (emb:*case-sensitivity* nil))
    (emb:execute-emb
     (merge-pathnames template-path
                      *template-directory*)
     :env (append env *global-context*))))

(defun render-json (object)
  (setf (headers *response* :content-type) "application/json")
  (encode-json object))



(defmacro with-layout ((&rest env-for-layout) &body body)
  (let ((layout-path (merge-pathnames *default-layout-path*
                                      *default-layout-directory*)))
    (when (pathnamep (car env-for-layout))
      (setf layout-path (pop env-for-layout)))

    `(let ((emb:*escape-type* :html)
           (emb:*case-sensitivity* nil))
       (emb:execute-emb
        (merge-pathnames ,layout-path
                         *template-directory*)
        :env (let ((*global-context* (get-global-context)))
               (list* :content (progn ,@body)
                      ,@env-for-layout
                      (get-global-context)))))))


;; Define functions that are available in templates.
(import '(codos.config:config
          codos.config:appenv
          codos.config:developmentp
          codos.config:productionp
          caveman2:url-for)
        emb:*function-package*)
