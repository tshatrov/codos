(in-package :cl-user)
(defpackage codos.web
  (:use :cl
        :caveman2
        :codos.config
        :codos.view
        :codos.db
        :datafly
        :sxql)
  (:export :*web*
           :codos-url-for))
(in-package :codos.web)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

;; (defroute "/" ()
;;   (with-layout (:title "CoDoS home")
;;     (render #P"index.tmpl")))

(defun brand-title (title)
  (format nil "~a - ~a" (config :page-title) title))

(defroute index "/" ()
  (with-layout (:title (brand-title "Home"))
    (render #P"index.tmpl")))

(defroute register "/codos/register/" ()
  (with-layout (:title (brand-title "Register new user"))
    (render #P"register.tmpl")))

;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))

;; helper functions

(defun codos-url-for (route-name &rest params)
  (let ((caveman2.app:*current-app* *web*))
    (apply #'caveman2:url-for route-name params)))
