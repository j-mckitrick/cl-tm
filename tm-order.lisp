;;; Define, populate, and export TM orders.

(in-package :tm)

;(declaim (optimize debug))

(defclass tm-order ()
  ((id :accessor tm-order.id :initarg :id :initform nil)
   (link :accessor tm-order.link :initarg :link :initform nil)
   (order-date :accessor tm-order.order-date :initarg :order-date :initform nil)
   (event-date :accessor tm-order.event-date :initarg :event-date :initform nil)
   (event-name :accessor tm-order.event-name :initarg :event-name :initform nil))
  (:documentation "Ticketmaster order base class."))

(defclass tm-order-details (tm-order)
  ((tfbits :accessor tm-order.tfbits :initform "")
   (status :accessor tm-order.status :initform "")
   (message-text :accessor tm-order.message-text :initform "")
   (amount :accessor tm-order.amount :initform 0)
   (venue :accessor tm-order.venue :initform "")
   (city :accessor tm-order.city :initform "")
   (state :accessor tm-order.state :initform "")
   (currency :accessor tm-order.currency :initform "")
   (time :accessor tm-order.event-time :initform "")
   (seats :accessor tm-order.seats :initform nil)
   (total-seats :accessor tm-order.total-seats :initform 0)
   (ticket-quantity :accessor tm-order.ticket-quantity :initform nil)
   (ticket-details :accessor tm-order.ticket-details :initform nil)
   (card-type :accessor tm-order.card-type :initform nil)
   (card-charges :accessor tm-order.card-charges :initform nil)
   (card-last4digits :accessor tm-order.card-last4digits :initform nil)
   (card-billing-address :accessor tm-order.card-billing-address :initform nil))
  (:documentation "Ticketmaster order details class."))

(defgeneric export-order-as-csv (order stream)
  (:documentation "Export an order to csv format."))

(defmethod export-order-as-csv ((order tm-order) (stream stream))
  (format stream "Not implemented.~%"))

(defmethod export-order-as-csv ((order tm-order-details) (stream stream))
  (format stream "~A, ~A, ~A, ~A, ~A, "
	  (tm-order.id order)
	  (tm-order.order-date order)
	  (tm-order.event-name order)
	  (tm-order.event-date order)
	  (tm-order.event-time order))
  (format stream "~A, ~A, ~A, "
	  (tm-order.venue order)
	  (tm-order.city order)
	  (tm-order.state order))
  (format stream "~A, ~A, ~A, ~A, ~A, "
	  (tm-order.currency order)
	  (tm-order.amount order)
	  (cdr (assoc "from" (cdr (assoc 1 (tm-order.seats order))) :test #'string=))
	  (cdr (assoc "thru" (cdr (assoc 1 (tm-order.seats order))) :test #'string=))
	  (tm-order.currency order))
  (let ((ticket-details (first (tm-order.ticket-details order))))
    (dolist (ticket-detail (cdr ticket-details))
      (format stream "~A, " (cdr ticket-detail))))
  (format stream "~A, ~A, ~A, ~A, ~A, "
	  (first (tm-order.ticket-quantity order))
	  (tm-order.card-charges order)
	  (tm-order.card-type order)
	  (tm-order.card-last4digits order)
	  (tm-order.card-billing-address order))
  (format stream "~A, ~A, ~A, ~A, ~A, "
	  (tm-order.tfbits order)
	  (tm-order.status order)
	  (tm-order.message-text order)
	  (tm-order.amount order)
	  (tm-order.total-seats order))
  (format stream "~%")
  (format t "Exported order to csv: ~A~%----------~%" (tm-order.id order)))

(defun rip-details-tables (page)
  "Return a list of all detailsTables in PAGE."
  (all-matches-as-strings *details-table-regex* page))

(defun rip-rows (table)
  "Return a list of all rows in TABLE."
  (all-matches-as-strings *rows-regex* table))

(defun rip-cells (row)
  "Return a list of all cells in ROW."
  (all-matches-as-strings *cells-regex* row))

(defun rip-cell-contents (cell)
  "Rip the contents of CELL."
  (multiple-value-bind (match contents)
      (scan-to-strings *cell-contents-regex* cell)
    (declare (ignorable match))
    (aref contents 0)))

;;; The following foo-table-fn functions take an ORDER and ROWS.
;;; Currently they are simply functions, but it might make sense
;;; to re-write them as methods.  Then again, it could also just
;;; be unnecessary overhead.

(defun charges-table-fn (order rows)
  "Rip charges table ROWS: date, order number, amount."
  (declare (tm-order-details order)
	   (list rows))
  (pop rows)				; ignore heading row
  (let ((cells (rip-cells (first rows))))
    (setf (tm-order.order-date order) (rip-cell-contents (first cells))
	  (tm-order.id order) (rip-cell-contents (second cells)))
    (register-groups-bind (currency amount)
	(*currency-regex* (rip-cell-contents (third cells)))
      (setf (tm-order.currency order) currency
	    (tm-order.amount order) amount))))

(defun event-table-fn (order rows)
  "Rip event table ROWS: event name, venue, city, state, event date and time."
  (declare (tm-order-details order)
	   (list rows))
  (pop rows)				; ignore heading row
  (let* ((detail-cell (first (rip-cells (first rows))))
	 (event-name (rip-table-item detail-cell "strong"))
	 (detail-cell-contents (rip-cell-contents (first rows))))
    #+tm-debug
    (progn
      (format t "Event name: ~A~%" event-name)
      (format t "Detail cell: ~A~%" detail-cell)
      (format t "Detail cell contents: ~A~%" detail-cell-contents))
    (setf (tm-order.event-name order) (regex-replace-all "[,\"]" event-name ""))
    (register-groups-bind (details) (*non-strong-details-regex* detail-cell-contents)
      #+tm-debug (format t "Event table details: '~A'~%" details)
      (register-groups-bind (venue city state date) (*event-location-regex* details)
	(setf date (regex-replace *date-src-regex* date *date-dest-regex*))
	(setf (tm-order.venue order) venue
	      (tm-order.city order) city
	      (tm-order.state order) state
	      (tm-order.event-date order) date))
      (register-groups-bind (time) (*time-regex* details)
	(setf (tm-order.event-time order) time)))))

(defun tickets-table-fn (order rows)
  "Rip tickets table ROWS: detailed ticket info for seating and charges.
FIXME: finish parsing all this data and saving it in order object!"
  (declare (tm-order-details order)
	   (list rows))
  (let ((headings (make-array 7 :element-type 'string :fill-pointer 0))
	(seating-group 1))
    (dolist (row rows)
      (let ((h-cells (all-matches-as-strings *h-cell-regex* row))
	    (d-cells (all-matches-as-strings *d-cell-regex* row)))
	;; build hash table of headings we expect
	(dolist (h-cell h-cells)
	  (let ((heading (rip-table-item (rip-cell-contents h-cell) "strong")))
	    #+tm-debug (format t "Found a ticket heading: ~A~%" heading)
	    (vector-push heading headings)))
	(register-groups-bind (from thru) (*seats-regex* row)
	  #+tm-debug (format t "Seats: ~A - ~A~%" from thru)
	  (let ((from (parse-integer (coerce from 'string)))
		(thru (parse-integer (coerce thru 'string)))
		seat-list)
	    (push (cons "from" from) seat-list)
	    (push (cons "thru" thru) seat-list)
	    (push (cons seating-group seat-list) (tm-order.seats order))
	    (if (> thru 0)
		(push (1+ (- thru from)) (tm-order.ticket-quantity order))
		(push 1 (tm-order.ticket-quantity order)))
	    (incf seating-group)))
	(let ((details
	       (loop
		  for heading across headings
		  for d-cell in d-cells
		  for contents = (rip-cell-contents d-cell) collect
		  #+tm-debug (format t "Found a ticket data cell: ~A~%" contents)
		  (cond
		    ((string= heading "Section")
		     (cons "section" contents))
		    ;; Seating row is inside a span
		    ((string= heading "Row")
		     (setf contents (rip-table-item contents "span"))
		     (cons "row" contents))
		    ((string= heading "Type")
		     (cons "type" contents))
		    ;; FIXME: currency notation for these 3
		    ((string= heading "Ticket Price")
		     (cons "ticketprice" contents))
		    ((string= heading "Building/Facility Charge")
		     (cons "buildingcharge" contents))
		    ((string= heading "Convenience Charge")
		     (cons "convcharge" contents))
		    ((string= heading "Description")
		     (cons "description" contents))))))
	  (unless (assoc seating-group (tm-order.ticket-details order))
	    (when details (push (cons seating-group details)
				(tm-order.ticket-details order)))))))))

(defun summary-table-fn (order rows)
  "Rip summary table ROWS: total number of seats.
All the charges are totaled here, but we really don't care about this yet."
  (declare (tm-order-details order)
	   (list rows))
  (pop rows)
  (dolist (row rows)
    (register-groups-bind (seat-count) (*seat-count-regex* row)
      (incf (tm-order.total-seats order)
	    (parse-integer (coerce seat-count 'string))))))

(defun billing-table-fn (order rows)
  "Rip billing table ROWS: credit card info."
  (declare (tm-order-details order)
	   (list rows))
  (pop rows)
  (let ((cells (rip-cells (first rows))))
    #+tm-debug
    (progn
      (format t "Card type: ~A~%" (rip-cell-contents (first cells)))
      (format t "Charges: ~A~%" (rip-cell-contents (second cells)))
      (format t "Last 4: ~A~%" (rip-cell-contents (third cells)))
      (format t "Billing address: ~A~%" (rip-cell-contents (fourth cells))))
    (setf (tm-order.card-type order) (rip-cell-contents (first cells)))
    (setf (tm-order.card-charges order) (rip-cell-contents (second cells)))
    (setf (tm-order.card-last4digits order) (rip-cell-contents (third cells)))
    (setf (tm-order.card-billing-address order) (rip-cell-contents (fourth cells)))))

(defparameter *table-roles*
  (list
   (cons "(?is)order date.*order \\#.*amount" #'charges-table-fn)
   (cons "(?is)event" #'event-table-fn)
   (cons "(?is)section.*row.*ticket\\s+price" #'tickets-table-fn)
   (cons "(?is)item.*charge" #'summary-table-fn)
   (cons "(?is)card\\s+type.*charge.*last\\s+4\\s+digits" #'billing-table-fn))
  "Regexes and function names for recognizing and parsing detailsTables.")

(defgeneric rip-order-details (order page)
  (:documentation "Rip all the details tables and populate an order."))

(defmethod rip-order-details ((order tm-order-details) (page string))
  "Populate ORDER with details ripped from PAGE."
  #+tm-debug (format t "Ripping order details.~%")
  (dolist (table (rip-details-tables page))
    (let* ((rows (rip-rows table))
	   (table-fn
	    (loop for (regex . fn) in *table-roles*
	       when (scan regex (first rows)) ; identify the role of this table
	       return #'(lambda (order) (funcall fn order rows)))))
      ;; Invoke the closure to rip table details.
      (funcall table-fn order))))

(defun rip-ticket-url (page)
  "Return the url embedded in the event handler that will open the ticket pdf."
  (do-register-groups (button-attrs button-content) (*div/button-regex* page)
    (declare (ignorable button-content))
    #+tm-debug
    (progn
      (format t "Button attributes: ~A~%" button-attrs)
      (format t "Button contents: ~A~%" button-content))
    (multiple-value-bind (whole-match regs)
	(scan-to-strings *onclick-regex* button-attrs)
      (when whole-match
	#+tm-debug (format t "Found a handler: ~A~%" (aref regs 0))
	(return-from rip-ticket-url (aref regs 0))))))

(defun rip-ticket-pdf (order page)
  "Rip the link to the ticket and fetch the pdf file."
  #+tm-debug (format t "Ripping ticket pdf.~%")
  (let* ((validation #(80 68 70 45 49))
	 (pdf (get-refreshing-page (rip-ticket-url page) "tix.pdf" validation))
	 (filename (concatenate 'string "/tmp/cl-"
				(substitute #\- #\/ (tm-order.id order)) ".pdf")))
    (if pdf
	(multiple-value-bind (type sub-type)
	    (get-content-type *saved-headers*)
	  (if (and (string= type "application")
		   (string= sub-type "pdf"))
	      (progn
		(save-binary filename pdf)
		(format t "Saved pdf ticket.~%"))
	      (progn
		(format t "Did not save - pdf content type: ~A/~A~%" type sub-type)
		(scan-for-error-messages pdf))))
	(progn
	  (format t "Did not save pdf: No download link found.~%")
	  (scan-for-status-messages page)))))
