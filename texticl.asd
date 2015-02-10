;;; -*- Lisp -*-
(defpackage #:texticl-system  (:use #:cl #:asdf))
(in-package #:texticl-system)

;; the araneida dependency is purely for html-escape
(defsystem texticl
    :depends-on (cl-ppcre araneida)
    :components ((:file "defpackage")
		 (:file "texticl" :depends-on ("defpackage"))))
