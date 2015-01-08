(in-package :cl-user)
(defpackage codos.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :codos.config)

(setf (config-env-var) "APP_ENV")

(defparameter *application-root*   (asdf:system-source-directory :codos))
(defparameter *static-directory*   (merge-pathnames #P"static/" *application-root*))
(defparameter *template-directory* (merge-pathnames #P"templates/" *application-root*))

(defparameter 1forms:*form-template-directory* (merge-pathnames #P"forms/" *template-directory*))

(defconfig :common
  `(:databases ((:maindb :sqlite3 :database-name ":memory:"))
    :top-bar-title "CoDoS"
    :main-title "CoDoS"
    :page-title "CoDoS"
    ))

(defconfig |development|
  '())

(defconfig |production|
  '())

(defconfig |test|
  '())

(load (asdf:system-relative-pathname :codos "settings.lisp")
      :if-does-not-exist nil)

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (asdf::getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
