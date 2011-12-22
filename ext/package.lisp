;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(in-package :cl-user)

(defpackage :planks.ext
  (:use
    :common-lisp)
  (:export
    :make-spinlock))
