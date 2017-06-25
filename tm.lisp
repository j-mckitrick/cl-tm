(defpackage :tm (:use :cl :drakma :cl-ppcre))

(in-package :tm)

;(declaim (optimize debug))

;; PPCRE NOTES:
;; ALL-MATCHES-AS-STRINGS - returns list of matching strings
;; SCAN-TO-STRINGS - m-v-b matching string, array of registers (best for one match)
;; DO-REGISTER-GROUPS - iterates matches, binds only registers
;; REGISTER-GROUPS-BIND - bind registers for one match

(defparameter *current-page* nil)
(defparameter *login-seed* nil)
(defparameter *cookie-jar* (make-instance 'cookie-jar))
(defparameter *logged-in-p* nil)
(defparameter *expected-orders* nil)
(defparameter *current-table* nil)
(defparameter *current-orders* nil)

(setf *header-stream* *standard-output*)

(defun get-secure-page (page name)
  (setf *current-page*
	(http-request (concatenate 'string "https://www.ticketmaster.com" page)
		      :cookie-jar *cookie-jar*))
  (with-open-file
      (s (pathname (concatenate 'string "/tmp/tm-" name ".html"))
	 :direction :output :if-exists :supersede)
    (format s *current-page*))
  (format t "Saved ~A~%" name))

(defun get-login-seed ()
  (register-groups-bind (v)
      ("(?i)<input.*?name=['\"]v['\"]\\s+value=['\"]([^'\"]*)['\"]([^>]*?)/>"
       *current-page*)
    (format t "Seed: ~A~%" v)
    (setf *login-seed* v)))

(defun post-login ()
  (setf *current-page*
	(http-request "https://www.ticketmaster.com/member"
		      :method :post
		      :parameters (list (cons "v" *login-seed*)
					(cons "email_address" "daniellesusan@aol.com")
					(cons "password" "maiseybaby1")) 
		      :cookie-jar *cookie-jar*))
  (with-open-file
      (s (pathname "/tmp/tm-login-result.html")
	 :direction :output :if-exists :supersede)
    (format s *current-page*)))

(defun get-order-history ()
  (get-secure-page "/member/order_history" "order_history")
  (multiple-value-bind (match order-count)
    (scan-to-strings "(?i)<strong>[-0-9]+</strong>\\s+[(]of\\s+([0-9]+)[)]"
     *current-page*)
    (declare (ignorable match))
    ;(format t "Order count match: '~A'~%" match)
    ;(format t "Order count: '~A'~%" order-count)
    (setf *expected-orders* (parse-integer (aref order-count 0)))))

(defun rip-table (attr-name attr-value ndx)
  (declare (ignore ndx))
  (dolist (table
	    (all-matches-as-strings "(?is)<table([^>]*)>(.*?)<\/table>"
				    *current-page*))
    ;;(format t "Checking table: ~A~%" table)
    (when (scan-to-strings (format nil "~A=\"~A\"" attr-name attr-value) table)
      (return-from rip-table table))))

(defun rip-table-item (container tag ndx)
  (let ((matches
	 (all-matches-as-strings
	  (concatenate 'string "(?is)<" tag "(?:[^>]*)>(.*?)<\/" tag ">")
	  container))
	(ndx-match nil))
    ;(format t "Table items: ~A~%" matches)
    (when matches
      (setf ndx-match (elt matches ndx))
      (multiple-value-bind (match regs)
	  (scan-to-strings
	   (concatenate 'string "(?is)<" tag "(?:[^>]*)>(.*?)<\/" tag ">")
	   ndx-match)
	(declare (ignorable match))
	(aref regs 0)))))

(defun rip-table-items (container tag)
  ;;(format t "Table: ~A~%" *current-table*)
  (let ((orders nil))
    (do-register-groups (row-contents)
	((concatenate 'string "(?is)<" tag "(?:[^>]*)>(.*?)<\/" tag ">")
	 container)
      (format t "Do a scan: ~A~%" row-contents)
      (unless (scan "<th([^>]*)>.*?<\/th>" row-contents)
	(let ((order-date (rip-table-item row-contents "td" 0))
	      (order-cell (rip-table-item row-contents "td" 1))
	      (event-cell (rip-table-item row-contents "td" 2))
	      (event-date (rip-table-item row-contents "td" 3)))
	  (when (and order-cell event-cell)
	    (let ((order-numb (rip-table-item order-cell "a" 0))
		  (event-name (rip-table-item event-cell "strong" 0)))
	      (register-groups-bind (order-link)
		  ("<a\\s+.*?href=[\"']?([^\"' >]*)[\"']?[^>]*>(.*?)<\/a>"
		   order-cell)
		(format t "Order date: ~A~%" order-date)
		(format t "Order cell: ~A~%" order-cell)
		(format t "Event cell: ~A~%" event-cell)
		(format t "Event date: ~A~%" event-date)
		(format t "Order numb: ~A~%" order-numb)
		(format t "Event name: ~A~%" event-name)
		(format t "Order link: ~A~%" order-link)
		(push (list order-link order-numb order-date event-date event-name)
		      orders)))))))
    orders))

(defun rip-orders (table)
  ;;(format t "Table: ~A~%" *current-table*)
  (let ((orders nil))
    (do-register-groups (row-contents)
	((concatenate 'string "(?is)<tr(?:[^>]*)>(.*?)<\/tr>")
	 table)
      ;(format t "Do a scan: ~A~%" row-contents)
      (unless (scan "<th([^>]*)>.*?<\/th>" row-contents)
	(let ((order-date (rip-table-item row-contents "td" 0))
	      (order-cell (rip-table-item row-contents "td" 1))
	      (event-cell (rip-table-item row-contents "td" 2))
	      (event-date (rip-table-item row-contents "td" 3)))
	  (when (and order-cell event-cell)
	    (let ((order-numb (rip-table-item order-cell "a" 0))
		  (event-name (rip-table-item event-cell "strong" 0)))
	      (register-groups-bind (order-link)
		  ("<a\\s+.*?href=[\"']?([^\"' >]*)[\"']?[^>]*>(.*?)<\/a>"
		   order-cell)
		#- (and)
		(progn
		  (format t "Order date: ~A~%" order-date)
		  (format t "Order cell: ~A~%" order-cell)
		  (format t "Event cell: ~A~%" event-cell)
		  (format t "Event date: ~A~%" event-date)
		  (format t "Order numb: ~A~%" order-numb)
		  (format t "Event name: ~A~%" event-name)
		  (format t "Order link: ~A~%" order-link))
		(push (list order-link order-numb order-date event-date event-name)
		      orders)))))))
    orders))

(defun export-order (order)
  (setf *current-page*
	(http-request (concatenate 'string "https://www.ticketmaster.com" (first order))
		      :cookie-jar *cookie-jar*)))

(defun do-all ()
  (unless *logged-in-p*
    (get-secure-page "/member" "login")
    (get-login-seed)
    (post-login)
    (get-secure-page "/member" "my_tm")
    (setf *logged-in-p* t))
  (get-order-history)
  (setf *current-table* (rip-table "class" "detailsTable" 1))
  (setf *current-orders* (rip-orders *current-table*))
  (format t "Orders: ~S~%" *current-orders*)
  (format t "Expected orders: ~A~%" *expected-orders*)
  (format t "Got ~A order detail links on this page.~%" (length *current-orders*))
  ;(export-order (first *current-orders*))
  )
