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
  (:shadow :defroute)
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

(defun brand-title (title)
  (format nil "~a - ~a" (config :page-title) title))

(defun post-params ()
  (body-parameter *request*))

(defmacro out (title-env template &optional template-env)
  (unless (and (listp title-env) (eql (car title-env) :title))
    (setf title-env `(:title ,title-env)))
  (destructuring-bind (title . layout-env) (cdr title-env)
    `(with-layout (:title (brand-title ,title) ,@layout-env)
       (render ,template ,template-env))))

(defmacro defroute (&rest args)
  (typecase (car args)
    (symbol
     (destructuring-bind (name routing-rule lambda-list &rest body) args
       `(caveman2:defroute ,name ,routing-rule ,lambda-list
         (with-connection (db)
           ,@body))))
    (list
     (destructuring-bind (routing-rule lambda-list &rest body) args
       `(caveman2:defroute ,routing-rule ,lambda-list
         (with-connection (db)
           ,@body))))
    (t `(defroute (,(car args)) ,@(cdr args)))))

(defun js-list (&rest paths)
  (mapcar (lambda (path) (list :path path)) paths))

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
    (out (:title (getf hub :title) :hub-id (getf hub :id)) #P"hub.tmpl" hub)))

(defroute new-document ("/document/new/" :method '(:post :get)) ()
  (let ((form (make-instance 'doc-settings-form)))
    (when (eql (request-method *request*) :post)
      (bind-form form 'post-params))
    (process-form form
        (out "New document" #P"docsettings.tmpl"
             `(:form ,(render-form form)))
      (redirect (url-for 'user-profile :login (getf (get-user-info) :login)))
      )))

(defroute user-profile ("/u/:login") (&key login)
  (db-let (user :user (where (:= :login login)))
      (throw-code 404)
    (out (format nil "Profile: ~a" (getf user :login)) #P"userprofile.tmpl"
         (get-full-user-data user))))

(defroute view-document ("/d/:slug") (&key slug)
  (db-let (document :document (where (:= :slug slug)))
      (throw-code 404)
    (out (:title (getf document :title)
          :extrajs (js-list "doc.js"))
         #P"document.tmpl" (get-document-data document))))

(defroute viewset-settings ("/codos/settings/viewsets/") ()
   (unless (getf (get-user-info) :adminp)
     (throw-code 403))
   (out "Viewset settings" #P"viewsets.tmpl"
        `(:viewfields ,(retrieve-all (select :* (from :viewfield)))
          :viewsets ,(get-all-viewsets))))

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
