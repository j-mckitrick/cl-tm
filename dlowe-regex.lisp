(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *regex-macro-character* #/)

  (defun read-regex-string (stream)
    (let ((eof (gensym "EOF")))
      (with-output-to-string (str)
        (loop for prev-char = nil then cur-char
           for cur-char = (read-char stream nil eof)
           until (or (eql cur-char eof)
                     (and (not (eql prev-char #))
                          (eql cur-char *regex-macro-character*)))
           do (write-char cur-char str)

           finally (when (eql cur-char eof)
                     (error "Unexpected end-of-file while reading regex"))))))

  (defun read-regex-options (stream)
    (let ((eof (gensym "EOF")))
      (loop for char = (read-char stream nil eof)
         until (or (eql char eof)
                   (not (alphanumericp char)))
         collect char)))

  (defun read-regex (stream char arg)
    (declare (ignore char arg))
    (let ((pattern (read-regex-string stream))
          (options (read-regex-options stream)))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (cl-ppcre:create-scanner ,pattern
                                  :case-insensitive-mode ,(member #i options)
                                  :multi-line-mode ,(member #m options)
                                  :single-line-mode ,(member #s options)
                                  :extended-mode ,(member #x options)))))

  (defun %enable-regex-reader-syntax ()
    (set-dispatch-macro-character ## *regex-macro-character* #'read-regex)
    (values)))

(defmacro enable-regex-reader-syntax ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    (%enable-regex-reader-syntax)))
#|
;; This allows syntax such as #/foo/im

(defclass regex-result ()
  ((target :accessor target-of :initarg :target)
   (pattern :accessor pattern-of :initarg :pattern)
   (match-start :accessor match-start-of :initarg :match-start)
   (match-end :accessor match-end-of :initarg :match-end)
   (reg-starts :accessor reg-starts-of :initarg :reg-starts)
   (reg-ends :accessor reg-ends-of :initarg :reg-ends)))

(defun regref (result idx)
  (assert result (result) "NIL regex-result passed to regref")
  (cond
    ((zerop idx)
      (subseq (target-of result)
              (match-start-of result)
              (match-end-of result)))
    ((null (aref (reg-starts-of result) (1- idx)))
     nil)
    (t
      (subseq (target-of result)
              (aref (reg-starts-of result) (1- idx))
              (aref (reg-ends-of result) (1- idx))))))

(defun scan (regex target &key start end)
  (assert target nil "Target for SCAN is NIL")
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (cl-ppcre:scan regex target :start (or start 0) :end (or end (length target)))
    (when match-start
      (make-instance 'regex-result
                     :target target
                     :pattern regex
                     :match-start match-start
                     :match-end match-end
                     :reg-starts reg-starts
                     :reg-ends reg-ends))))
|#
;; Use like:
;;  (scan #/foo(o+)/i "Foooo") => #
;;  (regref * 0) => Foooo
;;  (regref ** 1) => oo
