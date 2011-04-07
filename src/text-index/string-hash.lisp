;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-
;;;;;
;;;;; string-hash.lisp
;;;;;
;;;;; based largely on materials released by franz.inc text-indexing tech-corner seminar 
;;;;; under the following license terms:
;;;;;
;;;;; copyright (c) 2006, 2007 Franz Inc, Oakland, CA - All rights reserved.
;;;;;
;;;;; Permission is granted for any use whatsoever (commercial or
;;;;; non commercial) so long as the Franz Inc. copyright and this
;;;;; notice is preserved.


(defpackage :planks.hash
  (:use :common-lisp)
  (:nicknames :string-hash)
  (:documentation "separated out to provide room for additional hashing optimizations or
   approaches we may be potentially interested in down the road")
  (:export
    #:make-string-hash
    #:string-hash-get
    #:string-hash-set
    #:string-hash-pushnew
    #:string-hash-map
    #:string-hash-count
    #:string-hash-clear))

(in-package :planks.hash)

(defstruct ht
  size
  vec
  entries
  threshold)

(defstruct entry
  key
  value
  next)

;; size should be a prime for best results
(defun make-string-hash (&key (size 67) (rehash-threshold 0.80))
  (make-ht :size size 
	   :vec (make-array size)
	   :entries 0
	   :threshold (truncate (* size rehash-threshold))))

(defun string-hash-clear (ht)
  (setf (ht-vec ht) (make-array (ht-size ht)))
  (setf (ht-entries ht) 0)
  nil)

(defun string-hash-count (ht)
  (ht-entries ht))

(defun strcmp (string1 string2 start2 len)
  (declare (optimize (speed 3) (safety 0) #+ignore(debug 0))
	   (simple-string string1 string2)
	   (fixnum start2 len))
  (if (/= (length string1) len)
      (return-from strcmp nil))
  (dotimes (n len)
    (declare (fixnum n))
    (if (char/= (schar string1 n) (schar string2 start2))
	(return-from strcmp nil))
    (incf start2))
  t)

(defmacro hash (key start end size)
  `(the fixnum (mod (sxhash (subseq ,key ,start ,end))
		    (the fixnum ,size))))

(defmacro while (cond &body body)
  `(do () ((not ,cond)) ,@body))

;; Returns the entry node
(defun string-hash-get-1 (ht string start end)
  (declare ;; (optimize (speed 3 (safety 0) (debug 0))
	   (simple-string string)
	   (fixnum start end))
  (let* ((slot (hash string start end (ht-size ht)))
	 (node (svref (ht-vec ht) slot))
	 (len (- end start)))
    (declare (fixnum len))
    (while node
      (let ((key (entry-key node)))
	(declare (simple-string key))
	(if (strcmp key string start len)
	    (return-from string-hash-get-1 node))
	(setf node (entry-next node))))
    nil))

(defun string-hash-get (ht string &key (start 0) (end (length string)) default)
  (declare (optimize (speed 3) (safety 0))
    (simple-string string)
    (fixnum start end))
  (let ((node (string-hash-get-1 ht string start end)))
    (if node
      (entry-value node)
      default)))


;; FIXME: Handle replacement (although I don't need it for the text indexing
;; stuff).
(defun string-hash-set-1 (ht string start end value)
  (declare ;; (optimize (speed 3) (safety 0) (debug 0))
	   (simple-string string)
	   (fixnum start end))
  (let ((slot (hash string start end (ht-size ht)))
	(vec (ht-vec ht)))
    (setf (svref vec slot)
      (make-entry :key (subseq string start end)
		  :value value
		  :next (svref vec slot)))
    (let ((count (1+ (the fixnum (ht-entries ht)))))
      (declare (fixnum count))
      (setf (ht-entries ht) count)
      (if (> count (ht-threshold ht))
        #+sbcl (sb-impl::rehash ht)
        #-sbcl (rehash ht)))
    value))

(defun string-hash-set (ht string value &key (start 0) (end (length string)))
  (string-hash-set-1 ht string start end value))

;; Returns 't' if a new node was created.  nil otherwise.
(defun string-hash-pushnew (ht string value &key (start 0) (end (length string))
                             (test #'equal))
  (declare ;; (optimize (speed 3) (safety 0) (debug 0))
    (simple-string string)
    (fixnum start end))
  (let ((node (string-hash-get-1 ht string start end)))
    (if (null node) ;; todo condify - this was quick if* removal
      (progn
        (string-hash-set-1 ht string start end (list value))
        t)
      (if (not (funcall test (car (entry-value node)) value))
        (push value (entry-value node))
        nil))))

(defun string-hash-map (function ht)
;;  (declare (optimize (speed 3) (safety 0) #+ignore(debug 0)))
  (let ((vec (ht-vec ht)))
    (dotimes (n (length vec))
      (let ((node (svref vec n)))
	(while node
	  (funcall function (entry-key node) (entry-value node))
	  (setf node (entry-next node)))))))

;; FIXME:
;;  we're doing a bunch of consing by re-making nodes.
(defun rehash (ht)
;;  (declare (optimize (speed 3) (safety 0) #+ignore(debug 0)))
  (format t "resizing hash table.~%")
  (let* ((newsize (* (ht-size ht) 2))
	 (newht (make-string-hash :size newsize :rehash-threshold 2)))
    (string-hash-map #'(lambda (k v) (string-hash-set newht k v)) ht)
	  
    (setf (ht-vec ht) (ht-vec newht))
    (setf (ht-size ht) newsize)
    (setf (ht-threshold ht) (* (ht-threshold ht) 2))
    nil))

(defun chain-length (entry)
  (let ((count 0))
    (while entry
      (incf count)
      (setf entry (entry-next entry)))
    count))
    
(defun analyze (ht)
  (let ((count 0)
	(vec (ht-vec ht))
	(maxchain 0))
    (dotimes (n (length vec))
      (let ((node (svref vec n)))
	(when node
	  (incf count)
	  (let ((cl (chain-length node)))
	    (if (> cl maxchain)
		(setf maxchain cl))))))     
    (format t "~d slots out of ~d used.~%" 
	    count (ht-size ht))
    (format t "load factor ~d~%" (float (/ count (ht-size ht))))
    (format t "longest chain: ~d~%" maxchain)
    (let ((lens (make-array (1+ maxchain) :initial-element 0))
	  (probes 0))
      (dotimes (n (length vec))
	(let ((cl (chain-length (svref vec n))))
	  (incf (svref lens cl))
	  (dotimes (n cl)
	    (incf probes (1+ n)))))
      (dotimes (n (length lens))
	(format t "Slots with chain len ~d: ~d~%"
		n (svref lens n)))
      (format t "total probes: ~d~%" probes)
      (format t "average probes: ~d~%" (float (/ probes (ht-entries ht)))))))




(defun test-string-hash ()
  #-sbcl (gc t)
  #+sbcl (sb-ext:gc :full t)
  (let ((ht (make-string-hash :size 1000000))
         (x 0))
    (time
      (with-open-file (f "/Volumes/u/dan/src/dev/gs/ebu/planks/src/text-index/english-words.txt")
        (let (line)
          (while (setf line (read-line f nil nil))
            (string-hash-set ht line x)
            (incf x)))))
    (assert (eql (ht-entries ht) 144400))))

;; HASH> (test-string-hash)
;; Evaluation took:
;;   0.306 seconds of real time
;;   0.307234 seconds of total run time (0.180452 user, 0.126782 system)
;;   [ Run times consist of 0.101 seconds GC time, and 0.207 seconds non-GC time. ]
;;   100.33% CPU
;;   854,133,273 processor cycles
;;   34,038,560 bytes consed


;;; using lookup3 hash:         ;; C function
;;;   average probes: 1.249945
;;;   longest chain: 6
;;;   real time:  46,433 msec
;;;
;;; using univ-hash:           ;; lisp code
;;;   longest chain: 8
;;;   average probes: 1.256672
;;;   real time:  20,735 msec
;;;
;;; using sxhash:
;;;   longest chain: 11
;;;   average probes: 1.326746
;;;   real time:  5,851 msec

;;;;;
;; Local Variables:
;; indent-tabs: nil
;; outline-regexp: ";;[;]+"
;; End:
;;;;;
