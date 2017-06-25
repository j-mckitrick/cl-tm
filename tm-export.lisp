;;; Export TM orders.

(in-package :tm)

;(declaim (optimize debug))

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

