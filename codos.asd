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
               :cl-base32

               ;; HTML Template
               :cl-emb
               :cl-markup

               ;; for CL-DBI
               :datafly
               :sxql)
  :components ((:module "src"
                :serial t
                :components
                ((:file "config")
                 (:file "crypto")
                 (:file "db")
                 (:file "view")
                 (:file "forms-lib")
                 (:file "models")
                 (:file "forms")
                 (:file "web")
                 (:file "main"))))
  :description "Collaborative Document Store"
  :in-order-to ((test-op (load-op codos-test))))
