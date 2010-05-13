(defpackage scrapr-system (:use :cl :asdf))
(in-package :scrapr-system)

(defsystem scrapr
  :depends-on (:drakma :cl-ppcre)

  :components
  ((:file "scrapr")))
