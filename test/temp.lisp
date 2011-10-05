
;; tests from rucksack to think about

(defparameter *format-strings* 
  ;; Different ways of printing integers.
  '("~R" "~:R" "... ~R" "~D"))

(defun shuffle (array)
  (loop with n = (array-dimension array 0)
        repeat n
        for i = (random n)
        for j = (random n)
        when (/= i j)
        do (rotatef (aref array i) (aref array j))))


(defun check-size (btree expected)
  (format t "~&Counting~%")
  (let ((count (btree-nr-values btree)))
    (unless (=  count expected)
      (error "Wrong btree size - expected ~A, got ~A."
             expected count))))

(defun check-order (btree)
  (format t "~&Checking order and balance~%")
  (rs::check-btree btree)
  (format t " and keys~%")
  (rs::check-bnode-keys btree (rs::btree-root btree)))

(defun check-contents (btree)
  (format t "~&Checking contents~%")
  (map-btree btree
             (lambda (key value)
               (unless (string= value (format nil "~R" key))
                 (error "Value mismatch: Expected ~S, got ~S."
                        (format nil "~R" key) value)))))

(defmacro with-transaction* ((&rest args) &body body)
  `(with-transaction ,args
     (prog1 (progn ,@body)
       (format t "~&Committing..."))))

(defun test-btree (&key (n 20000) (node-size 100) (delete (floor n 10))
                        check-contents)
  ;; Create a rucksack with a btree of size N that maps random
  ;; integers to the equivalent strings as a cardinal English number.
  ;; Use node size NODE-SIZE for the btree.
  ;; If DELETE is not NIL, delete and reinsert that number of elements
  ;; as well.
  (let ((array (make-array n :initial-contents (loop for i below n collect i))))
    (shuffle array)
    (with-rucksack (rucksack *test-suite* :if-exists :supersede)
      (with-transaction* ()
        (format t "~&Inserting~%")
        (let ((btree (make-instance 'btree :value= 'string-equal
                                    :max-node-size node-size)))
          (loop for key across array
                for i from 1
                when (zerop (mod i 1000))
                do (format t "~D " i)
                do (btree-insert btree key
                                 (format nil (first *format-strings*) key)))
          (add-rucksack-root btree rucksack))))
    (with-rucksack (rucksack *test-suite*)
      (with-transaction ()
        (let ((btree (first (rucksack-roots rucksack))))
          (check-order btree)
          (check-size btree n)
          (when check-contents
            (check-contents btree))))
      (when delete
        (shuffle array)
        (setq array (subseq array 0 delete))
        (shuffle array)
        (with-transaction* ()
          (format t "~&Deleting~%")
          (let ((btree (first (rucksack-roots rucksack))))
            (dotimes (i delete)
              (when (zerop (mod (1+ i) 100))
                (format t "~D " (1+ i)))
              (btree-delete-key btree (aref array i)))
            (check-order btree)
            (check-contents btree)))
        (with-transaction* ()
          (let ((btree (first (rucksack-roots rucksack))))
            (check-order btree)
            (check-size btree (- n delete))
            (when check-contents
              (check-contents btree))
            (format t "~&Reinserting~%")
            (shuffle array)
            (dotimes (i delete)
              (when (zerop (mod (1+ i) 1000))
                (format t "~D " (1+ i)))
              (let ((key (aref array i)))
                (btree-insert btree key (format nil "~R" key))))))
        (with-transaction ()
          (let ((btree (first (rucksack-roots rucksack))))
            (check-order btree)
            (check-size btree n)
            (when check-contents
              (check-contents btree)))))))
  :ok)

;;
;; Btrees with non-unique keys

(defun check-non-unique-contents (btree)
  (format t "~&Checking contents~%")
  (map-btree btree
             (lambda (key value)
               (let ((strings (loop for format-string in *format-strings*
                                    collect (format nil format-string key))))
                 (unless (member value strings :test #'string-equal)
                   (error "Value mismatch: Expected one of ~S for ~S, got ~S."
                          strings key value))))))


(defun test-non-unique-btree (&key (n 20000) (node-size 100) (delete (floor n 10))
                                   check-contents)
  ;; Create a rucksack with a btree of size N (N must be a multiple of 4) that
  ;; maps random integers to four different equivalent strings (in Roman and
  ;; English notation).
  ;; Use node size NODE-SIZE for the btree.
  ;; If DELETE is not NIL, it must be a multiple of 4; delete that number of
  ;; elements as well.
  (let* ((nr-formats (length *format-strings*))
         (array-size (floor n nr-formats))
         (array (make-array array-size
                            :initial-contents (loop for i from 1 to array-size collect i))))
    (assert (zerop (mod n nr-formats)))
    (assert (zerop (mod delete nr-formats)))
    (shuffle array)
    (with-rucksack (rucksack *test-suite* :if-exists :supersede)
      (with-transaction* ()
        (format t "~&Inserting~%")
        (let ((btree (make-instance 'btree :value= 'string-equal
                                    :max-node-size node-size
                                    :unique-keys-p nil)))
          (loop for key across array
                for i from 1
                when (zerop (mod i 200))
                do (format t "~D " i)
                do (loop for format-string in *format-strings*
                         do (btree-insert btree key (format nil format-string key))))
          (add-rucksack-root btree rucksack))))
    (with-rucksack (rucksack *test-suite*)
      (with-transaction ()
        (let ((btree (first (rucksack-roots rucksack))))
          (check-order btree)
          (check-size btree n)
          (when check-contents
            (check-non-unique-contents btree))))
      (when delete
        (shuffle array)
        (setq array (subseq array 0 (floor delete nr-formats)))
        (shuffle array)
        (with-transaction* ()
          (format t "~&Deleting~%")
          (let ((btree (first (rucksack-roots rucksack))))
            (loop for i below (floor delete nr-formats)
                  do (loop for j below nr-formats
                           do (when (zerop (mod (+ j (* nr-formats i)) 10))
                                (format t "~D " (+ j (* nr-formats i))))
                           do (let* ((key (aref array i))
                                     (from-end (oddp key))
                                     (index (if from-end
                                                j
                                              (- nr-formats (1+ j))))
                                     (format-string (elt *format-strings* index))
                                     (value (format nil format-string key)))
                                (btree-delete btree key value
                                              :if-does-not-exist :error))))
            (check-order btree)
            (check-size btree (- n delete))
            (check-non-unique-contents btree)))
        (with-transaction* ()
          (let ((btree (first (rucksack-roots rucksack))))
            (check-order btree)
            (check-size btree (- n delete))
            (when check-contents
              (check-non-unique-contents btree))
            (format t "~&Reinserting~%")
            (shuffle array)
            (dotimes (i (floor delete nr-formats))
              (when (zerop (mod (1+ i) 10))
                (format t "~D " (1+ i)))
              (let ((key (aref array i)))
                (loop for format-string in *format-strings*
                      do (btree-insert btree key (format nil format-string key)))))))
        (with-transaction ()
          (let ((btree (first (rucksack-roots rucksack))))
            (check-order btree)
            (check-size btree n)
            (when check-contents
              (check-non-unique-contents btree)))))))
  :ok)

(defun btree-stress-test (&key (n 1000))
  (loop for i below n
        do (print i)
        do (test-non-unique-btree :n 1600 :node-size 10 :delete 1500)))

(defun test-btree-map (&key (display t) min max include-min include-max
                            (order :ascending))
  ;; Print out the contents of the btree.
  (with-rucksack (rucksack *test-suite*)
    (with-transaction ()
      (let ((btree (first (rucksack-roots rucksack))))
        (map-btree btree
                   (lambda (key value)
                     (when display
                       (format t "~&~D -> ~A~%" key value)))
                   :min min
                   :include-min include-min
                   :max max
                   :include-max include-max
                   :order order)))))

