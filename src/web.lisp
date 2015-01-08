(in-package :cl-user)
(defpackage codos.web
  (:use :cl
        :caveman2
        :codos.config
        :codos.view
        :codos.db
        :1forms
        :codos.forms
        :codos.models
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


(defun test-url-for (route &rest params)
  (let ((caveman2.app:*current-app* *web*))
    (apply 'url-for route params)))
;;
;; Routing rules

;; (defroute "/" ()
;;   (with-layout (:title "CoDoS home")
;;     (render #P"index.tmpl")))

(defun brand-title (title)
  (format nil "~a - ~a" (config :page-title) title))

(defun post-params ()
  (body-parameter *request*))

(defmacro out (title template &optional template-env)
  `(with-layout (:title (brand-title ,title))
     (render ,template ,template-env)))

(defroute index "/" ()
  (out "Home" #P"index.tmpl"
       `(:hubs ,(list-table :hub (order-by :id)))
       ))

(defroute register ("/codos/register/" :method :GET) ()
  (let ((form (make-instance 'register-form)))
    (out "Register new user" #P"register.tmpl"
         `(:form ,(render-form form)))))

(defroute ("/codos/register/" :method :POST) ()
  (let ((form (make-instance 'register-form)))
    (bind-form form 'post-params)
    (process-form form
        (out "Register new user" #P"register.tmpl"
             `(:form ,(render-form form)))
      (redirect (url-for 'login)))))

(defroute login ("/codos/login/" :method :GET) ()
  (let ((form (make-instance 'login-form)))
    (out "Log in" #P"login.tmpl"
         `(:form ,(render-form form)))))

(defroute ("/codos/login/" :method :POST) ()
  (let ((form (make-instance 'login-form)))
    (bind-form form 'post-params)
    (process-form form
        (out "Log in" #P"login.tmpl"
             `(:form ,(render-form form)))
      (redirect (url-for 'index)))))

(defroute logout ("/codos/logout/") ()
  (logout-user)
  (redirect (url-for 'index)))

(defroute view-hub ("/h/:slug") (&key slug)
  (db-let (hub :hub (where (:= :slug slug)))
      (throw-code 404)
    (out slug #P"hub.tmpl" hub)))

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
