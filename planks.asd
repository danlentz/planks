;;;;; -*- mode: lisp; syntax: common-lisp; coding: utf-8; base: 10; -*-

(in-package :cl-user)

(asdf:defsystem #:planks 
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :serial t
  :components ((:module :corpus
                 :components ((:static-file "english-words.txt")))
                (:module :src
                  :components ((:file "btree-protocol")
                                (:file "btree")
                                (:file "map-btree")
                                (:file "file-btree")
                                (:file "heap-btree")
                                (:file "view")
                                (:file "btree-class")
                                (:file "object-btree")
                                (:file "persistent-objects")
                                (:file "string-hash")
                                (:file "text-index"))))
  :depends-on (:rucksack
                :ironclad
                :bordeaux-threads
                :babel
                :closer-mop))


		
