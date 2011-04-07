;; copyright (c) 2006, 2007 Franz Inc, Oakland, CA - All rights reserved.
;;
;; Permission is granted for any use whatsoever (commercial or
;; non commercial) so long as the Franz Inc. copyright and this
;; notice is preserved.

(defpackage :text-index
  (:use :lisp :excl)
  (:export
   ;; class
   #:text-index
   ;; functions/methods/macros
   #:open-text-index
   #:close-text-index
   #:with-text-index
   #:delete-text-index
   #:flush-cache
   #:index-word
   #:tokenize-string
   #:make-word-list
   #:index-string
   #:index-stream
   #:index-file
   #:lookup-word
   #:lookup-text
   #:lookup-phrase
   #:lookup-expr
   #:map-text-index
   #:string-downcase-in-place
   ;; constant
   #:*max-word*
   ))

(in-package :text-index)

(defvar user::*text-index-dir* (pathname "text-index/"))

(defvar *freetext-output-stream* nil)

(eval-when (compile load eval)
  (defconstant *max-word* 200))

(eval-when (compile load eval)
  (when (not (find-package :db.ac))
    ;; for btree and quicktab
    (require :acache
	     (or
	      ;;** ignore-errors needed because resolution of .. in / will
	      ;;** signal an error 
	      ;; For users of the `acl' module in conjunction w/agraph:
	      (ignore-errors (probe-file "../../acache/acache.fasl"))
	      ;; needed for the rpm build on linux
	      (ignore-errors (probe-file "../../../acache/acache.fasl"))
	      "acache-2.1.5.fasl")))
  (require :streamp) ;; for memcpy
  (if (not (find-package :string-hash))
      (cond ((probe-file 
	      (merge-pathnames "string-hash.cl" user::*text-index-dir*))
             (load (compile-file-if-needed
                    (merge-pathnames "string-hash.cl" user::*text-index-dir*))))
            ((probe-file "string-hash.cl")
             (load (compile-file-if-needed 
                   "string-hash.cl"))))))

(defparameter *default-max-intermediate-files* 30)

(defclass text-index ()
  (
   (filename :accessor filename :initarg :filename)
   (info-file :accessor info-file :initarg :info-file)
   (big-btrees :accessor big-btrees :initform nil)
   (big-btree-ids :accessor big-btree-ids :initform nil)
   (small-btrees :accessor small-btrees :initform nil)
   (small-btree-ids :accessor small-btree-ids :initform nil)
   (seq :accessor seq :initform 0)
   (buffer-pool :accessor buffer-pool :initarg :buffer-pool)
   (hash :accessor hash :initarg :hash)
   (cache-size :accessor cache-size :initarg :cache-size :initform nil)
   (cache-metric :accessor cache-metric :initform 0)
   (max-intermediate-files :accessor max-intermediate-files 
			   :initarg :max-intermediate-files
			   :initform *default-max-intermediate-files*)
   (tokenizer :accessor tokenizer :initarg :tokenizer))
  )

(deftype ausb8 () '(simple-array (unsigned-byte 8) (*)))

(defmacro make-usb8 (size &rest rest)
  `(make-array ,size :element-type '(unsigned-byte 8) ,@rest))

(defun open-text-index (filename &key (cache-size (* 64 1024 1024))
				      (if-exists :open)
				      (if-does-not-exist :error)
				      (max-intermediate-files 
				       *default-max-intermediate-files*)
				      (tokenizer #'tokenize-string))
  (if (and (probe-file filename) (eq if-exists :supersede))
      (delete-text-index filename))
  
  (let* ((f (open filename :direction :io :if-exists if-exists
		  :if-does-not-exist if-does-not-exist))
	 (bp (db.btree:make-btree-buffer-pool))
	 (idx (make-instance 'text-index
		:filename filename
		:info-file f
		:buffer-pool bp
		:cache-size (truncate (/ cache-size 2))
		:max-intermediate-files max-intermediate-files
		:tokenizer tokenizer
		:hash (string-hash:make-string-hash
		       :size (truncate (/ cache-size 300))))))
    (when (> (file-length f) 0)
      (read-info-file f idx)
      (setf (big-btrees idx) (open-btrees idx (big-btree-ids idx)))
      (setf (small-btrees idx) (open-btrees idx (small-btree-ids idx))))
    
    idx))

(defun read-info-file (f idx)
  (destructuring-bind (seq big-ids small-ids)
      (read f)
    (setf (seq idx) seq)
    (setf (big-btree-ids idx) big-ids)
    (setf (small-btree-ids idx) small-ids)))
  

(defmacro id-to-filename (idx id)
  `(format nil "~a.~a" (filename ,idx) ,id))

(defmacro open-text-btree (index id &rest args)
  (let ((idx (gensym)))
    `(let ((,idx ,index))
       (db.btree:open-btree (id-to-filename ,idx ,id)
			    :cache-size (truncate (cache-size ,idx) 6)
			    :buffer-pool (buffer-pool ,idx)
			    ,@args))))
			    
(defun open-btrees (idx ids)
  (mapcar #'(lambda (id) (open-text-btree idx id)) ids))


;; Information which needs to be retained:
;; sequence number
;; ids of big btrees
;; ids of small btrees
(defun update-info-file (idx)
  (let ((f (info-file idx)))
    (file-position f 0)
    (write (list (seq idx) (big-btree-ids idx) (small-btree-ids idx)) 
	   :stream f
	   :pretty nil)
    (terpri f)))

(defmethod close-text-index ((idx text-index))
  (flush-cache idx)
  (setf (hash idx) nil)
  (dolist (bt (big-btrees idx))
    (db.btree:close-btree bt))
  (setf (big-btrees idx) nil)
  (dolist (bt (small-btrees idx))
    (db.btree:close-btree bt))
  (setf (small-btrees idx) nil)
  (close (info-file idx))
  (setf (info-file idx) nil))
  
(defmacro with-text-index ((var filename &rest options) &body body)
  `(let ((,var (open-text-index ,filename ,@options)))
     (unwind-protect (progn ,@body)
       (close-text-index ,var))))

(defun delete-text-index (filename &key if-does-not-exist)
  (handler-case 
      (let ((idx (make-instance 'text-index
		   :filename filename)))
	(when (> (file-length filename) 0)
	  (with-open-file (f filename)
	    (read-info-file f idx)
	    (dolist (id (small-btree-ids idx))
	      (delete-file (id-to-filename idx id)))
	    (dolist (id (big-btree-ids idx))
	      (delete-file (id-to-filename idx id)))))
	(delete-file filename))
    (file-error (c)
      (if (or (/= (syscall-error-errno c) excl::*enoent*)
	      (eq if-does-not-exist :error))
	  (error c)))))

;;;; 

;; Only include words with >= 3 chars
(defparameter *stop-words*
    '("and" "are" "but" "for"
      "into" "not" "such"
      "that" "the" "their" "then" "there" "these"
      "they" "this" "was" "will" "with"))

(defun bad-word-p (string start end)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (simple-string string))
  (let ((len (- end start)))
    (declare (fixnum len))
    (or (< len 3) 
	(> len #.*max-word*)
	(dolist (stopword *stop-words*)
	  (if (string-hash::strcmp stopword string start len)
	      (return t))))))

(defmacro fast-char-downcase (char)
  `(svref #.(let ((arr (make-array 65536)))
	      (dotimes (n 65536)
		(let ((char (code-char n)))
		  (setf (svref arr n) (char-downcase char))))
	      arr)
	  (char-code ,char)))

(defun string-downcase-in-place (string)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (dotimes (n (length string))
    (declare (fixnum n))
    (setf (schar string n) 
      (fast-char-downcase (schar string n)))))

;; Case sensitive.  downcase strings before calling to effect case
;; insensitivity.   Bad words are never indexed.  However, characters that 
;; otherwise would have been ignored by index-string are accepted as-is.
(defmethod index-word ((index text-index) word value
		       &key (start 0) (end (length word)))
  (index-word-1 index word start end value))

(defun index-word-1 (index word start end value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (not (bad-word-p word start end))
    (let* ((hash (hash index))
	   (metric (cache-metric index)))
      (declare (fixnum metric value start end))
      (if* (string-hash:string-hash-pushnew hash word value
					    :start start
					    :end end)
	 then (incf metric (+ 16 (the fixnum (- end start)))))
      (incf metric 12)
      (if* (>= metric (the fixnum (cache-size index)))
	 then (message "autoflushing due to metric: ~a~%" metric)
	      (flush-cache index)
	 else (setf (cache-metric index) metric))
      nil)))

;; Tokenizer

;; This is the default tokenizer.
;; Runs of letters, numbers are considered words.
;; dashes, underscores, and periods are also considerd to be
;; parts of words, except for at the beginning or ending of a word.

(defparameter *valid-border-chars* 
    (let ((res (make-array 65536 :element-type 'bit)))
      (dotimes (n 65536)
	(setf (aref res n)
	  (if (alphanumericp (code-char n)) 1 0)))
      res))

(defparameter *valid-inner-chars* 
    (let ((res (make-array 65536 :element-type 'bit)))
      (dotimes (n 65536)
	(setf (aref res n)
	  (let ((char (code-char n)))
	    (if (or (alphanumericp char)
		    (char= char #\.)
		    (char= char #\-)
		    (char= char #\_))
		1 0))))
      res))

(defparameter *valid-border-chars-wild* 
    (let ((res (make-array 65536 :element-type 'bit)))
      (dotimes (n 65536)
	(let ((char (code-char n)))
	  (setf (aref res n)
	    (if (or (alphanumericp char) (char= char #\?) (char= char #\*))
		1
	      0))))
      res))

(defparameter *valid-inner-chars-wild* 
    (let ((res (make-array 65536 :element-type 'bit)))
      (dotimes (n 65536)
	(setf (aref res n)
	  (let ((char (code-char n)))
	    (if (or (alphanumericp char)
		    (char= char #\.)
		    (char= char #\-)
		    (char= char #\_)
		    (char= char #\?)
		    (char= char #\*))
		1 0))))
      res))


(defun tokenize-string (string start end outbuf wild)
  (declare (optimize (speed 3))
	   (simple-string string outbuf)
	   (fixnum start end))
  (let ((n 0)
	(vbc (if wild *valid-border-chars-wild* *valid-border-chars*))
	(vic (if wild *valid-inner-chars-wild* *valid-inner-chars*))
	char)
    (declare (fixnum n)
	     ((simple-array bit 65536) vbc vic))
    
    (macrolet ((valid-border-char-p (char)
		 `(eq 1 (aref vbc (char-code ,char))))
	       (invalid-border-char-p (char)
		 `(zerop (aref vbc (char-code ,char))))
	       (invalid-inner-char-p (char)
		 `(zerop (aref vic (char-code ,char)))))
      ;; Advance to valid border char
      (while (< start end)
	(setf char (schar string start))
	(when (valid-border-char-p char)
	  ;; Got one.  Now copy in an any inner chars.
	  (while (< start end) 
	    (setf char (schar string start))
	    (if (invalid-inner-char-p char)
		(return))
	    (when (< n *max-word*)
	      (setf (schar outbuf n) (fast-char-downcase char))
	      (incf n))
	    (incf start))
	  (if (>= n *max-word*)
	      ;; The word was too long.  Return the next word (if any)
	      ;; instead.
	      (return-from tokenize-string 
		(tokenize-string string start end outbuf wild)))
	  ;; Backspace over any non-border chars
	  (while (invalid-border-char-p (schar outbuf (1- n)))
	    (decf n))
	  (return (values n start)))
	(incf start)))))

;; Utility function.  Not used internally.
(defmethod make-word-list ((index text-index) string &key wild)
  (declare (optimize (speed 3))
	   (simple-string string))
  (let ((end (length string))
	(word-buf (make-string *max-word*))
	(pos 0)
	(tokenizer (tokenizer index))
	word-len
	res)
    (declare (dynamic-extent word-buf))
    (loop
      (multiple-value-setq (word-len pos)
	(funcall tokenizer string pos end word-buf wild))
      (if (null word-len)
	  (return))
      (if (not (bad-word-p word-buf 0 word-len))
	  (push (subseq word-buf 0 word-len) res)))
    (nreverse res)))
    

(defmethod index-string ((index text-index) string value
			 &key (start 0) (end (length string)))
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (simple-string string))
  (index-string-1 index string start end value))

(defun index-string-1 (index string start end value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((word (make-array #.*max-word* :element-type 'character))
	(tokenizer (tokenizer index))
	len)
    (declare (dynamic-extent word)
	     (fixnum len))
    (loop
      (multiple-value-setq (len start)
	(funcall tokenizer string start end word nil))
      (if (null len)
	  (return))
      (index-word-1 index word 0 len value))))

(defmethod index-stream ((index text-index) stream value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((line (make-array 1024 :element-type 'character))
	pos)
    (declare (dynamic-extent line)
	     (fixnum pos))
    (while (setf pos (read-line-into line stream nil))
      ;;(message "line: ~a~%" (subseq line 0 pos))
      (index-string-1 index line 0 pos value))))
  

(defmethod index-file ((index text-index) filename value)
  (with-open-file (f filename)
    (index-stream index f value)))

;;; Storage

(defun open-new-btree (idx type)
  (let ((id (seq idx))
	bt)
    (incf (seq idx))
    (setf bt (open-text-btree idx id 
			      :if-exists :supersede
			      :if-does-not-exist :create
			      :partial-writes t))
    (ecase type
      (:small 
       (push id (small-btree-ids idx))
       (push bt (small-btrees idx)))
      (:big
       (push id (big-btree-ids idx))
       (push bt (big-btrees idx)))
      (:super
       ))
    (update-info-file idx)
    (values bt id)))


(defmethod flush-cache ((index text-index))
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((hash (hash index))
	 (count (string-hash:string-hash-count hash))
	 list)
    (declare (cons list))
    
    (when (> count 0)
      (message "Flushing... ~d words in cache.~%" count)
      
      (string-hash:string-hash-map #'(lambda (k v) (push (cons k v) list))
				   hash)

      (message "sorting...~%")
      (setf list (sort list #'string< :key #'car))

      (message "~%storing...~%")
      (let ((bt (open-new-btree index :small)))
	 (dolist (pair list)
	   (store bt (car pair) (cdr pair))))
      (message "~%")
      
      (if (> (length (small-btrees index)) (max-intermediate-files index))
	  (merge-small-btrees index))
      
      (string-hash:string-hash-clear hash)
      (setf (cache-metric index) 0)
      (message "flush complete.~%"))))

(defun merge-small-btrees (idx)
  (message "Merging small btrees...~%")
  (let ((big (open-new-btree idx :big)))
    (db.btree:merge-btrees big (small-btrees idx))
    (mapc #'db.btree:close-btree (small-btrees idx))
    (dolist (id (small-btree-ids idx))
      (delete-file (id-to-filename idx id)))
    (setf (small-btrees idx) nil)
    (setf (small-btree-ids idx) nil)
    (update-info-file idx))
  (message "~%")
  
  (if (> (length (big-btrees idx)) (max-intermediate-files idx))
      (merge-big-btrees idx)))

(defun merge-big-btrees (idx)
  (message "Merging big btrees...~%")
  (multiple-value-bind (super super-id)
      (open-new-btree idx :super)
    (db.btree:merge-btrees super (big-btrees idx))
    (mapc #'db.btree:close-btree (big-btrees idx))
    (dolist (id (big-btree-ids idx))
      (delete-file (id-to-filename idx id)))
    (setf (big-btrees idx) (list super))
    (setf (big-btree-ids idx) (list super-id))
    (update-info-file idx))
  (message "~%"))

;; There shouldn't be any pressing need to use this
(defmethod super-flush ((idx text-index))
  (if (small-btrees idx)
      (merge-small-btrees idx))
  (if (> (length (big-btrees idx)) 1)
      (merge-big-btrees idx)))

;;;;;;  Lookups ;;;;;;;;;;;;;

(defmacro with-cursor ((cur btree) &body body)
  `(let ((,cur (db.btree:create-cursor ,btree)))
     (unwind-protect (progn ,@body)
       (db.btree:unbind-cursor ,cur))))

(defun arrays-match-p (a1 s1 e1 a2 s2 e2)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array (unsigned-byte 8) (*)) a1 a2)
	   (fixnum s1 e1 s2 e2))
  (while (and (< s1 e1) (< s2 e2))
    (if (not (eq (aref a1 s1) (aref a2 s2)))
	(return-from arrays-match-p))
    (incf s1)
    (incf s2))
  (and (eq s1 e1) (eq s2 e2)))


(defun utf8-to-string (vec start end out)
  (declare (optimize (speed 3))
	   (fixnum start end)
	   (ausb8 vec))
  (if (null out)
      (setf out (make-string *max-word*)))
  (let ((outpos 0))
    (declare (fixnum outpos)
	     (simple-string out))
    (while (< start end)
      (macrolet ((nextbyte ()
		   `(prog1 (aref vec start)
		      (incf start)))
		 (outchar (code)
		   `(progn 
		      (setf (schar out outpos) (code-char ,code))
		      (incf outpos)))
		 (lowsix (value)
		   `(logand ,value #x3f)))
	(let ((b (nextbyte)))
	  (if* (<= b #x7f)
	     then (outchar b)
	   elseif (eq (logand b #xe0) #xc0)
	     then ;; 2 byte encoding
		  (outchar (logior
			    (ash (logand b #b11111) 6)
			    (lowsix (nextbyte))))
	     else ;; 3 byte encoding
		  (outchar (logior
			    (ash (logand b #xf) 12)
			    (ash (lowsix (nextbyte)) 6)
			    (lowsix (nextbyte))))))))
    (values out outpos)))

;; vec should be at least (* 6 *max-word*) in size since the
;; longest utf8 encoding can be 6 bytes (for 32-bit unicode).
(defun string-to-utf8 (word start end vec)
  (declare (optimize (speed 3) (safety 0))
	   (fixnum start end)
	   (simple-string word)
	   (ausb8 vec))

  (if (> (- end start) (length vec))
      (error "word too long"))
  
  (let ((pos 0)
	code)
    (declare (fixnum pos))

    (macrolet ((put (val)
		 `(progn (setf (aref vec pos) ,val)
			 (incf pos))))
      
      (while (< start end)
	(setf code (char-code (schar word start)))
	(if* (<= code #x7f)
	   then ;; simple ascii
		(put code)
	 elseif (<= code #x7ff)
	   then ;; two byte encoding
		(put (logior #xc0 (ash code -6)))
		(put (logior #x80 (logand code #x3f)))
	   else ;; three byte encoding
		(put (logior #xe0 (ash code -12)))
		(put (logior #x80 (logand #x3f (ash code -6))))
		(put (logior #x80 (logand #x3f code))))
	(incf start))
      
      pos)))

(defmacro with-string-to-utf8 ((word wstart wend vec veclen) 
			       &body body)
  (let ((tmpvec (gensym)))
    `(let* ((,tmpvec (make-usb8 (* 6 *max-word*)))
	    (,veclen (string-to-utf8 ,word ,wstart ,wend ,tmpvec))
	    (,vec ,tmpvec))
       (declare (dynamic-extent ,tmpvec)
		(fixnum ,veclen))
       ,@body)))

(defmacro with-all-btrees ((bt index) &body body)
  (let ((idx (gensym))
	(l1 (gensym))
	(l2 (gensym)))
    `(let* ((,idx ,index)
	    (,l1 (big-btrees ,idx))
	    (,l2 (small-btrees ,idx))
	    ,bt)
       (loop
	 (if* ,l1
	    then (setf ,bt (pop ,l1))
	  elseif ,l2
	    then (setf ,bt (pop ,l2))
	    else (return))
	 
	 ,@body))))

(defmethod lookup-word ((index text-index) word &key (wstart 0)
						     (wend (length word))
						     wild)
  (declare (optimize (speed 3) (safety 0))
	   (simple-string word))

  (if (> (- wend wstart) *max-word*)
      (return-from lookup-word nil))

  (if wild
      (return-from lookup-word (lookup-word-wild index word wstart wend)))
  
  ;; Look in the cache first
  (let ((res (copy-list (string-hash:string-hash-get (hash index) word 
						     :start wstart 
						     :end wend))))
    ;; Now the btrees
    (with-string-to-utf8 (word wstart wend word wlen)
      (with-all-btrees (bt index)
	(setf res (nconc res (lookup-word-1 bt word wlen))))
      (dedupe res))))


;; 'word' is a usb8 here.
(defun lookup-word-1 (btree word len)
  (declare (optimize (speed 3)))
  (let (res)
    (with-cursor (cur btree)
      (when (db.btree:position-cursor-ext cur word 0 len :prime t)
	(loop
	  (multiple-value-bind (k v)
	      (db.btree:cursor-next cur)
	    (declare (ausb8 k v))
	    (if (or (null k) (not (arrays-match-p k 0 (length k)
						  word 0 len)))
		(return))
	    (setf res (nconc res (extract-integers v (ash (length v) -2))))))
	res))))

(defun generate-re (word start end)
  (declare (optimize (speed 3))
	   (simple-string word)
	   (fixnum start end))
  (let ((buf (make-string *max-word*))
	(bufn 0)
	res)
    (declare (dynamic-extent buf)
	     (fixnum bufn))
    (macrolet ((addbuf (char)
		 `(progn 
		    (setf (schar buf bufn) ,char)
		    (incf bufn)))
	       (flushbuf ()
		 `(when (not (zerop bufn))
		    (push (subseq buf 0 bufn) res)
		    (setf bufn 0))))
      
      (push :sequence res)
      (push :start-anchor res)

      (while (< start end)
	(let ((char (schar word start)))
	  (if* (char= char #\*)
	     then (flushbuf)
		  (push '(:greedy-repetition 0 nil :everything) res)
	   elseif (char= char #\?)
	     then (flushbuf)
		  (push :everything res)
	     else (addbuf char)))
	(incf start))
      (flushbuf)
      (push :end-anchor res)
      ;;(message "generated re is ~s~%" (reverse res))
      (compile-re (nreverse res) :return nil))))

(defun lookup-word-wild (index word start end)
  (multiple-value-bind (found whole prefix)
      (match-re "^([^*?]*)[*?].*$" word :start start :end end)
    (declare (ignore whole))
    ;;(message "matched: ~a, prefix: ~s~%" found prefix)
    (when (null found)
      ;;(message "No wildcards.  Reverting to standard lookup.~%")

      (return-from lookup-word-wild 
	(lookup-word index word :wstart start :wend end)))
    
    ;; Make sure all data is in the btrees.
    (flush-cache index)

    (with-string-to-utf8 (prefix 0 (length prefix) prefixvec prefixlen)
      ;;(message "prefixlen is ~d~%" prefixlen)
      ;;(message "word is ~s~%" (subseq word start end))
      (let ((re (generate-re word start end))
	    res)
	(with-all-btrees (bt index)
	  (setf res 
	    (nconc res (lookup-word-wild-btree bt re prefixvec
					       prefixlen))))
	(dedupe res)))))

;; prefix must be usb8.  zero-length usb8 is okay
(defun lookup-word-wild-btree (btree re prefix prefixlen)
  (declare (optimize (speed 3)))
  (let ((buf (make-string *max-word*))
	res)
    (declare (dynamic-extent buf))
    
    (with-cursor (cur btree)
      (db.btree:position-cursor-ext cur prefix 0 prefixlen :prime t)
      
      (loop
	(multiple-value-bind (key kstart kend value vstart vend)
	    (db.btree:cursor-next-ext cur)
	  (if (null key)
	      (return))
	  (multiple-value-bind (key klen)
	      (utf8-to-string key kstart kend buf)
	    ;;(message "considering key ~s~%" (subseq key 0 klen))
	    (when (match-re re key :end klen)
	      ;;(message "accepted.~%")
	      (let ((v (subseq value vstart vend)))
		(declare (ausb8 v))
		(setf res 
		  (nconc res (extract-integers v (ash (length v) -2)))))))))
      res)))

(defmethod lookup-text ((index text-index) text &key wild
						     if-no-good-words)
  (declare (optimize (speed 3) (safety 0))
	   (simple-string text))
  (let ((word (make-array #.*max-word* :element-type 'character))
	(start 0)
	(end (length text))
	(first t)
	(tokenizer (tokenizer index))
	seen-good-word
	len
	res)
    (declare (fixnum start end len)
	     (dynamic-extent word))
    
    (loop
      (if (and (null first) (null res))
	  (return))
      (multiple-value-setq (len start)
	(funcall tokenizer text start end word wild))
      (if (null len)
	  (return))
      (when (not (bad-word-p word 0 len))
	(setf seen-good-word t)
	(let ((res2 (lookup-word index word :wstart 0 :wend len :wild wild)))
	  (if* first
	     then (setf first nil)
		  (setf res res2)
	     else (setf res (integer-intersection res res2))))))
    (if (and (null seen-good-word) (eq if-no-good-words :error))
	(error "Search string consisted entirely of stopwords"))
    res))

(defmethod lookup-phrase ((index text-index) phrase map-function 
			  &key if-no-good-words)
  (let (res)
    (dolist (val (lookup-text index phrase :if-no-good-words if-no-good-words))
      (if (search phrase (funcall map-function val) :test #'string-equal)
	  (push val res)))
    (dedupe res)))


;; EXPR = string: Word(s) (ala lookup-text), possibly w/ wildcards
;;      = string surrounded by double quotes:  Phrase
;;      = (or EXPR1 ... EXPRn)
;;      = (and EXPR1 ... EXPRn)
(defmethod lookup-expr ((index text-index) expr phrase-map-function
			&key if-no-good-words)
  (lookup-expr-1 index expr phrase-map-function if-no-good-words))

(defun lookup-expr-1 (index expr phrase-map-function if-no-good-words)
  (etypecase expr
    (string ;; word(s) or phrase.
     (if* (prefixp "\"" expr)
	then (lookup-phrase index (subseq expr 1 (1- (length expr)))
			    phrase-map-function)
	else (lookup-text index expr :wild t 
			  :if-no-good-words if-no-good-words)))
    (list
     (ecase (car expr)
       (and
	(let ((first t)
	      res)
	  (dolist (expr (rest expr))
	    (let ((res1 (lookup-expr-1 index expr phrase-map-function
				       if-no-good-words)))
	      (when (null res1)
		(setf res nil)
		(return))
	      (if* first 
		 then (setf first nil)
		      (setf res res1)
		 else (setf res (integer-intersection res res1))
		      (if (null res)
			  (return)))))
	  res))
       (or
	(let (res)
	  (dolist (expr (rest expr))
	    (setf res 
	      (integer-union res 
			     (lookup-expr-1 index expr phrase-map-function
					    if-no-good-words))))
	  res))))))

;; 'func' must be a function of two arguments.  The first
;; will the be word and the second will be the list of values
(defmethod map-text-index ((idx text-index) func)
  (flush-cache idx)
  (with-all-btrees (bt idx)
    (with-cursor (cur bt)
      (db.btree:position-cursor cur nil :kind :first :prime t)
      (let ((buf (make-string *max-word*)))
	(loop
	  (multiple-value-bind (key kstart kend value vstart vend)
	      (db.btree:cursor-next-ext cur)
	    (declare (ausb8 key value))
	    (if (null key)
		(return))
	    (setf value (subseq value vstart vend))
	    (multiple-value-bind (keystring keystring-len)
		(utf8-to-string key kstart kend buf)
	      (funcall func (subseq keystring 0 keystring-len)
		       (extract-integers value (ash (length value) -2))))))))))

;;;;;;;;;;;;

(defun integer-intersection (a b)
  (declare (optimize (speed 3)))
  (if (or (null a) (null b))
      (return-from integer-intersection))
	       
  (let* ((lena (length a))
	 (lenb (length b))
	 (min (min lena lenb))
	 (q (db.quicktab:create-quicktab min))
	 n1 n2
	 (res nil))
    (if (< lena lenb)
	(setf n1 a n2 b)
      (setf n1 b n2 a))
    
    (dolist (e n1)
      (setf (db.quicktab:getquick e q) t))
    (dolist (e n2)
      (when (db.quicktab:getquick e q)
	(push e res)))
    res))

(defun integer-union (a b)
  (declare (optimize (speed 3)))
  
  (if (null a)
      (return-from integer-union b))
  
  (if (null b)
      (return-from integer-union a))
  
  (let ((q (db.quicktab:create-quicktab (+ (length a) (length b))))
	res)
    (dolist (x a)
      (when (not (db.quicktab:getquick x q))
	(setf (db.quicktab:getquick x q) t)
	(push x res)))
    (dolist (x b)
      (when (not (db.quicktab:getquick x q))
	(setf (db.quicktab:getquick x q) t)
	(push x res)))
    res))

;; May want to just use remove-duplicates when the list is less than
;; a certain length
(defun dedupe (list)
  (declare (optimize (speed 3)))
  (let ((h (db.quicktab:create-quicktab (length list)))
	res)
    (dolist (val list)
      (setf (db.quicktab:getquick val h) t))
    (db.quicktab:mapquick #'(lambda (k v) (declare (ignore v)) (push k res)) h)
    res))

(defun extract-integers (vec count)
  (declare (optimize (speed 3) (safety 0))
	   (type (simple-array (signed-byte 32) (*)) vec)
	   (fixnum count))
  (let (res)
    (dotimes (n count)
      (push (aref vec n) res))
    res))

;;;;;; Storage ;;;;;;;;;;;;

(defun store (btree word values)
  (declare (optimize (speed 3) (safety 0))
	   (simple-string word)
	   (cons values))
  (with-string-to-utf8 (word 0 (length word) key keylen)
    (let* ((vec-len (* (length values) 4))
	   (vec (allocate-vector vec-len))
	   (n 0))
      (declare (type (simple-array (signed-byte 32) (*)) vec)
	       (fixnum vec-len n))
      
      (dolist (val values)
	(setf (aref vec n) val)
	(incf n))
      
      (db.btree:set-btree-ext btree key 0 keylen vec 0 vec-len)
      
      (deallocate-vector vec))))

;;;;;

(defparameter *vectors* (make-array 10 :weak t))

(defun allocate-vector (size)
  (declare (optimize (speed 3) (safety 0) (debug 0))
	   (fixnum size))
  (let ((v *vectors*))
    (dotimes (n (length v))
      (declare (fixnum n))
      (let ((pair (aref v n)))
	(if* (and pair (>= (the fixnum (car pair)) size))
	   then (setf (aref v n) nil)
		(return-from allocate-vector (cdr pair))))))
  
  (make-array size :element-type '(unsigned-byte 8)))

(defun deallocate-vector (vec)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((v *vectors*)
	 (len (length v)))
    (declare (fixnum len))
    
    (dotimes (n len)
      (declare (fixnum n))
      (if* (null (aref v n))
	 then (setf (aref v n) (cons (length vec) vec))
	      (return-from deallocate-vector)))
    
    ;; Need to expand the vector
    (let ((new (make-array (* len 2) :weak t)))
      (dotimes (n len)
	(setf (aref new n) (aref v n)))
      (setf (aref new len) (cons (length vec) vec))
      (setf *vectors* new))))

;;;; Debug i/o

(defun message (format &rest args)
  (when *freetext-output-stream*
    (fresh-line *freetext-output-stream*)
    (apply #'format *freetext-output-stream* format args)
    (force-output *freetext-output-stream*)))

;;;; testing

#+ignore
(defun test ()
  (with-text-index (idx "test.idx" 
			:if-exists :supersede
			:if-does-not-exist :create)
    (index-word idx "tomboy" 1)
    (index-word idx "tomcat" 2)
    (index-word idx "bruce" 3)
    (index-word idx "sally" 7)
    (index-word idx "bubble" 4)
    (format t "Lookup of still-cached 'bruce': ~s~%"
	    (lookup-word idx "bruce"))
    (flush-cache idx)
    (index-word idx "bruce" 5)
    (format t "Lookup 'bruce' which is in cache and in btree: ~s~%"
	    (lookup-word idx "bruce"))
    (format t "Lookup tom*: ~s~%" (lookup-word idx "tom*" :wild t))
    (format t "Lookup *: ~s~%" (lookup-word idx "*" :wild t))
    (format t "Lookup *e: ~s~%" (lookup-word idx "*e" :wild t))
    (format t "Lookup b*e: ~s~%" (lookup-word idx "*e" :wild t))
    (format t "Lookup ?ubbl?: ~s~%" (lookup-word idx "?ubbl?" :wild t))
    (let* ((string1 "This is the story of bruce bubble")
	   (string2 "bruce liked bubble gum")
	   (func 
	    #'(lambda (v)
		(format t "map function called for value: ~a~%" v)
		(if* (= v 10)
		   then string1
		 elseif (= v 11)
		   then string2))))
      
      (index-string idx string1 10)
      (index-string idx string2 11)

      (format t "Lookup phrase test: ~s~%"
	      (lookup-phrase idx "bruce bubble" func))
	       
      (format t "expression lookup: ~s~%"
	      (lookup-expr idx "\"liked bubble\""  func)))))

#+ignore
(defun test (filename)
  (with-text-index (idx "test.idx" 
			:if-exists :supersede
			:if-does-not-exist :create
			:cache-size (* 1024 1024)
			:max-intermediate-files 10)
    (index-file idx filename 1)))

#+ignore
(defun test ()
  (with-text-index (idx "test.idx" :if-exists :supersede
			:if-does-not-exist :create))
  (with-text-index (idx "test.idx" :if-exists :supersede
			:if-does-not-exist :create)))

