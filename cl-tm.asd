;; -*- mode: lisp; encoding: utf-8 -*-
  ;:depends-on (:cl-ppcre :drakma)

(asdf:defsystem :cl-tm
  :name "cl-tm"
  :version "0.1"
  :serial t
  :components
  (;(:file "dlowe-regex")
   (:file "defpackage")
   (:file "tm-class")
   (:file "tm"))
  :depends-on (:cl-ppcre :drakma))
