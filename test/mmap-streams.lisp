;;;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-
;;;;;

(in-package :planks.btree)

(defparameter *btree* #P"/tmp/test.btree")

(defclass local-btree-class (btree-class)
  ((x :initarg :x :initform 3))
;;  (:metaclass btree-class)
  (:default-initargs :btree-path #P"/tmp/test.btree"))

(defclass oo (btree-object)
  ((x :initarg :x :initform 3)))

  (:metaclass btree-class))


(define-symbol-macro |bt| (or (find-btree *btree*)
                            (make-btree "/tmp/o.btree"
                              :class 'object-storage-btree)))


(defclass persistent-node (persistent-standard-object)
  ((s :initarg :s)
    (p :initarg :p)
    (o :initarg :o)
    (c :initarg :c))
  (:btree    "/tmp/o.btree")
  (:metaclass persistent-standard-class))


(make-instance 'persistent-node :s "subject" :p "predicate" :o "object" :c "context")


(defun test-simple-mmap-stream ()
  (let ((f (open "/tmp/a.txt" :direction :io :element-type '(unsigned-byte 8)
             :if-exists :overwrite :if-does-not-exist :create)))
    (with-open-stream (m (make-instance 'mmap-stream :base-stream f :mmap-size 5))
      (describe m)
      (stream-truncate m 7)
      (let ((code (char-code #\!)))
        (file-position m 1)
        (write-byte code m)
        (file-position m 1)
        (assert (= code (read-byte m))))
      (let ((buffer (make-array 3 :element-type '(unsigned-byte 8))))
        (file-position m 0)
        (write-sequence (sb-ext:string-to-octets "abc") m)
        (file-position m 0)
        (read-sequence buffer m)
        (assert (equalp (sb-ext:string-to-octets "abc") buffer) (buffer) "1 ~a" buffer))
      (let ((buffer (make-array 3 :element-type '(unsigned-byte 8))))
        (write-sequence (sb-ext:string-to-octets "def") m)
        (file-position m 3)
        (read-sequence buffer m)
        (assert (equalp (sb-ext:string-to-octets "def") buffer) (buffer) "2 ~a" buffer))
      (let ((buffer (make-array 3 :element-type '(unsigned-byte 8))))
        (write-sequence (sb-ext:string-to-octets "ghi") m)
        (file-position m 6)
        (read-sequence buffer m)
        (assert (equalp (sb-ext:string-to-octets "ghi") buffer) (buffer) "3 ~a" buffer))
      (let ((code (char-code #\#)))
        (write-byte code m)
        (file-position m (1- (file-position m)))
        (assert (= code (read-byte m))))
      (let ((buffer1 (make-array 20 :element-type '(unsigned-byte 8) :initial-element 1))
             (buffer2 (make-array 20 :element-type '(unsigned-byte 8))))
        (file-position m 0)
        (write-sequence buffer1 m)
        (file-position m 0)
        (read-sequence buffer2 m)
        (assert (equalp buffer1 buffer2) (buffer1 buffer2) "3 ~a")))))



