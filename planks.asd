(in-package :cl-user)

(asdf:defsystem #:planks 
  :author "Drew Crampsie <drewc@tech.coop>"
  :licence "MIT"
  :serial t
  :components ((:file "package")
                
                #+(and sbcl (or x86 x86-64))
                (:module :vendor
                  :components ((:file "sb-mmap")
                                (:file "sb-lset"))
                  :serial t)

                (:module :src
                  :components ((:file "btree-protocol")
                                (:file "btree")
                                (:file "btree-utils")
                                (:file "btree-search")
                                (:file "map-btree")
                                (:file "file-btree")
                                (:file "heap-btree")
                                (:file "view")
                                (:file "btree-class")
                                (:file "object-btree")
                                (:file "persistent-objects"))		
                  :serial t))

  :depends-on (:rucksack
                :ironclad
                :bordeaux-threads
                :trivial-garbage
                :babel
                :closer-mop))


		
