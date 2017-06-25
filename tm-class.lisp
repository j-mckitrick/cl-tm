(in-package :tm)

(defclass <tm-order> ()
  ((id :accessor id-of :initarg :id :initform nil)
   (link :accessor link-to :initarg :link :initform nil)
   (order-date :accessor order-date-of :initarg :order-date :initform nil)
   (event-date :accessor event-date-of :initarg :event-date :initform nil)
   (event-name :accessor event-name-of :initarg :event-name :initform nil))
  (:documentation "Ticketmaster order base class."))

(defclass <tm-order-details> (<tm-order>)
  ((tfbits :accessor tfbits-of :initarg :tfbits :initform nil)
   (message-text :accessor message-text-of :initarg :message-text :initform nil)
   (status :accessor status-of :initarg :status :initform nil))
  (:documentation "Ticketmaster order details class."))

(defgeneric export-order-as-csv (order stream)
  (:documentation "Export an order to csv format."))

(defmethod export-order-as-csv ((order <tm-order>) stream)
  (format stream "Not implemented.~%"))

(defmethod export-order-as-csv ((order <tm-order-details>) stream)
  (format stream "Not implemented.~%"))

(defun rip-details-tables (page)
  (all-matches-as-strings
   "(?is)<table\\s+class=.detailsTable.(?:[^>]*)>(.*?)</table>" page))

(defun rip-details-rows (table)
  (all-matches-as-strings "(?is)<tr(?:[^>]*)>(.*?)</tr>" table))

(defun rip-details-cells (row)
  (all-matches-as-strings "(?is)<t(?:h|d)(?:[^>]*)>(.*?)</t(?:h|d)>" row))

(defun rip-details-cell-contents (row)
  (multiple-value-bind (cell contents)
      (scan-to-strings "(?is)<t(?:h|d)(?:[^>]*)>(.*?)</t(?:h|d)>" row)
    (declare (ignorable cell))
    (when (> (length contents) 0)
      (aref contents 0))))

(defun charges-table-fn (rows order)
  ;(format t "Charges rows: ~A~%" rows)
  (pop rows)
  ;(format t "Charges remaining rows: ~A~%" rows)
  (let ((cells (rip-details-cells (car rows))))
    (setf (event-date-of order) "Hello!")
    (setf (order-date-of order) (rip-details-cell-contents (first cells)))
    (setf (id-of order) (rip-details-cell-contents (second cells)))))

(defun event-table-fn (rows order)
  (declare (ignore order))
  (format t "Event rows: ~A~%" rows))

(defun tickets-table-fn (rows order)
  (declare (ignore order))
  (format t "Tickets rows: ~A~%" rows))

(defun summary-table-fn (rows order)
  (declare (ignore order))
  (format t "Summary rows: ~A~%" rows))

(defun billing-table-fn (rows order)
  (declare (ignore order))
  (format t "Billing rows: ~A~%" rows))

(defparameter *table-roles* (list
			     (cons "(?is)order date.*order \\#.*amount"
				   #'charges-table-fn)
			     (cons "(?is)event"
				   #'event-table-fn)
			     (cons "(?is)section.*row.*ticket\\s+price"
				   #'tickets-table-fn)
			     (cons "(?is)item.*charge"
				   #'summary-table-fn)
			     (cons "(?is)card\\s+type.*charge.*last\\s+4\\s+digits"
				   #'billing-table-fn)))

(defgeneric rip-order-details (order page)
  (:documentation "Rip all the details tables and populate an order."))

(defmethod rip-order-details ((order <tm-order-details>) (page string))
  (let ((tables (rip-details-tables page)))
    (dolist (table tables)
      (let ((rows (rip-details-rows table)))
	(format t "Row 0: ~S~%" (first rows))
	(let ((table-fn
	       (loop for (regex . fn) in *table-roles*
		  when (all-matches-as-strings regex (first rows))
		  return #'(lambda () (funcall fn rows order) order))))
	  (format t "Table function: ~A~%" table-fn)
	  (let* ((new-order (funcall table-fn))
		 (compared (eq order new-order)))
	    (format t "Compared: ~A~%" compared))
	  (break "Order" order)
	  )))))

