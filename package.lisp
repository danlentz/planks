;;; -*- mode: common-lisp;   common-lisp-style: modern;    coding: utf-8; -*-

(in-package :cl-user)

(defpackage :planks.btree
  (:use :common-lisp)
  (:export
    
   ;; Btrees
    #:btree
    #:multi-btree   
    #:heap-btree
    #:single-file-btree
    #:object-storage-btree
    
   ;; Parameters
    #:btree-key<
    #:btree-key=
    #:btree-value=
    #:btree-max-node-size
    #:btree-unique-keys-p
    #:btree-key-type
    #:btree-value-type

   ;; Nodes
    #:btree-root
    #:btree-node
    #:btree-node-class

   ;; Functions
    #:make-btree
    #:open-btree
    #:find-btree
    #:close-btree    
    #:update-btree
    #:btree-search
    #:btree-insert
    #:map-btree
    #:add-function-btree
    #:find-function-btree
    #:btree-file-size
    
   ;; Conditions
    #:btree-error
    #:btree-search-error
    #:btree-insertion-error
    #:btree-key-already-present-error
    #:btree-type-error
    #:btree-error-btree
    #:btree-error-key
    #:btree-error-value))


