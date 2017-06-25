;;; -*- mode: lisp -*-

;;; DB-backed TM classes.

(in-package :cl-user)

(defpackage :cl-tm-asd
  (:use :cl :asdf))

(in-package :cl-tm-asd)

(defsystem :cl-tm
  :name "cl-tm"
  :version "0.1"
  :serial t
  :components
  (;(:file "dlowe-regex")
   (:file "defpackage")
   (:file "tm-globals")
   (:file "tm-orderdb")
   (:file "tm-export")
   (:file "tm-rip")
   (:file "tm-db")
   (:file "tm"))
  :depends-on (:cl-ppcre :drakma :clsql))
