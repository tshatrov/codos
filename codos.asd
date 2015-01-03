(in-package :cl-user)
(defpackage codos-asd
  (:use :cl :asdf))
(in-package :codos-asd)

(defsystem codos
  :version "0.1"
  :author "Timofei Shatrov"
  :license "MIT"
  :depends-on (:clack
               :caveman2
               :envy
               :cl-ppcre

               ;; HTML Template
               :cl-emb

               ;; for CL-DBI
               :datafly
               :sxql)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("config"))
                 (:file "config"))))
  :description "Collaborative Document Store"
  :in-order-to ((test-op (load-op codos-test))))
