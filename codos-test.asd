(in-package :cl-user)
(defpackage codos-test-asd
  (:use :cl :asdf))
(in-package :codos-test-asd)

(defsystem codos-test
  :author "Timofei Shatrov"
  :license "MIT"
  :depends-on (:codos
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:file "codos"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
