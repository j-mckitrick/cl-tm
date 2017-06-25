;;; Database functions.

(in-package :tm)

(defun tm-connect-db ()
  (clsql:connect '("litedata.sql") :database-type :sqlite3))
