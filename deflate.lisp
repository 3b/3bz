(in-package 3bz)

;; libz says these are enough entries for zlib as specified
(defconstant +max-tree-entries/len+ 852)
(defconstant +max-tree-entries/dist+ 592)
(defconstant +max-tree-size+ (+ +max-tree-entries/len+
                                +max-tree-entries/dist+))

;; low-bit tags for nodes in tree
(defconstant +ht-literal+ #b00)
(defconstant +ht-link/end+ #b01)
(defconstant +ht-len/dist+ #b10)
(defconstant +ht-invalid+ #b11)

;; 'end' code in lit/len alphabet
(defconstant +end-code+ 256)
;; first length code in lit/len alphabet
(defconstant +lengths-start+ 257)
;; last valid length (there are some extra unused values to fill tree)
(defconstant +lengths-end+ 285)
;; offset of length codes in extra-bits tables
(defconstant +lengths-extra-bits-offset+ 32)

(deftype ht-bit-count-type ()'(unsigned-byte 4))
(deftype ht-offset-type ()'(unsigned-byte 11))
(deftype ht-node-type ()'(unsigned-byte 16))
(deftype ht-node-array-type () `(simple-array ht-node-type (,+max-tree-size+)))


;; accessors/predicates/constructors for node in tree
;; low bits 00 = literal
;; low bits 01 = link flag, #x0001 = end, #xffff = invalid
;; low bits 10 = len/dist
;; (low bits 11 = invalid)

(declaim (inline ht-linkp ht-invalidp ht-endp ht-node-type
                 ht-link-bits ht-link-offset
                 ht-literalp ht-value
                 ht-link-node ht-literal-node ht-len-node ht-dist-node
                 ht-invalid-node ht-end-node))
(defun ht-linkp (node)
  (oddp node))
(defun ht-invalidp (node)
  (= node #xffff))
;; (usually will just check for link bits or link-offset = 0 for endp)
(defun ht-endp (node)
  (= node #x0001))
(defun ht-node-type (node)
  (ldb (byte 2 0) node))

;; for valid link, store 4 bits of bit-count, 11 bits of table base
(defun ht-link-node (bits index)
  (logior +ht-link/end+
          (ash bits 2)
          (ash index 6)))
(defun ht-link-bits (node)
  (ldb (byte 4 2) node))
(defun ht-link-offset (node)
  (ldb (byte 10 6) node))


(defun ht-literalp (node)
  (zerop (ldb (byte 2 0) node)))
(defun ht-len/dist-p (node)
  (= 1 (ldb (byte 2 0) node)))
;; literals, length and distance just store a value. for literals, it
;; is the code value, for len/dist it is index into base and
;; extra-bits arrays
(defun ht-value (node)
  (ldb (byte 14 2) node))

(defun ht-literal-node (value)
  (logior +ht-literal+
          (ash value 2)))

(defun ht-len-node (value)
  (assert (>= value +lengths-start+))
  (logior +ht-len/dist+
          ;; value stored in tree is offset so we can use single table
          ;; for extra-bits and base-values for lengths and distances
          (ash (+ +lengths-extra-bits-offset+
                  (if (>= value +lengths-start+)
                      (- value +lengths-start+)
                      value))
               2)))

(defun ht-dist-node (value)
  (logior +ht-len/dist+
          (ash value 2)))

(defun ht-invalid-node () #xffff)
(defun ht-end-node () #x0001)

(defstruct (huffman-tree (:conc-name ht-))
  (len-start-bits 0 :type ht-bit-count-type)
  (dist-start-bits 0 :type ht-bit-count-type)
  (dist-offset 0 :type ht-offset-type)
  (nodes (make-array +max-tree-size+ :element-type 'ht-node-type
                                     :initial-element (ht-invalid-node))
   :type ht-node-array-type))

(deftype code-table-type () '(simple-array (unsigned-byte 4) 1))

(defparameter *fixed-lit/length-table*
  (concatenate 'code-table-type
               (make-array (1+ (- 143 0)) :initial-element 8)
               (make-array (1+ (- 255 144)) :initial-element 9)
               (make-array (1+ (- 279 256)) :initial-element 7)
               (make-array (1+ (- 287 280)) :initial-element 8)))

(defparameter *fixed-dist-table*
  (coerce (make-array 32 :initial-element 5) 'code-table-type))

;; extra-bits and len/dist-bases store
(declaim (type (simple-array (unsigned-byte 4)
                             (#. (+ 29 +lengths-extra-bits-offset+)))
               *extra-bits*)
         (type (simple-array (unsigned-byte 16)
                             (#. (+ 29 +lengths-extra-bits-offset+)))
               *len/dist-bases*))
(defparameter *extra-bits*
  (concatenate
   '(simple-array (unsigned-byte 4) (61))
   (replace (make-array +lengths-extra-bits-offset+ :initial-element 0)
            #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
   #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0)))

;; base length value for each length/distance code, add to extra bits
;; to get length

(defparameter *len/dist-bases*
  (concatenate '(simple-array (unsigned-byte 16) (61))
               (replace (make-array +lengths-extra-bits-offset+ :initial-element 0)
                        #(1 2 3 4 5 7 9 13 17 25 33 49 65 97
                          129 193 257 385 513 769
                          1025 1537 2049 3073 4097 6145 8193
                          12289 16385 24577))
               #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99
                 115 131 163 195 227 258)))

(declaim (type (simple-array (unsigned-byte 15) (32768)) *bit-rev-table*))
(defparameter *bit-rev-table*
  (coerce (loop for i below (expt 2 15)
                collect (parse-integer
                         (reverse (format nil "~15,'0b" i)) :radix 2))
          '(simple-array (unsigned-byte 15) (*))))

(declaim (inline bit-rev))
(defun bit-rev (x bits)
  (declare (type (unsigned-byte 15) x))
  (ldb (byte bits (- 15 bits)) (aref *bit-rev-table* x)))

(defun build-tree-part (tree table tree-offset type)
  ;; # of entries of each bit size
  (let* ((counts (let ((a (make-array 16 :element-type '(unsigned-byte 11)
                                         :initial-element 0)))
                   (map nil (lambda (i) (when (plusp i) (incf (aref a i))))
                        table)
                   a))
         ;; first position of each used bit size
         (offsets (let ((c 0))
                    (declare (type (unsigned-byte 11) c))
                    (map '(simple-array (unsigned-byte 11) (16))
                         (lambda (a)
                           (prog1
                               (if (zerop a) 0 c)
                             (incf c a)))
                         counts)))
         ;; first code of each used bit size
         (code-offsets (let ((c 0))
                         (declare (type (unsigned-byte 17) c))
                         (map '(simple-array (unsigned-byte 16) (16))
                              (lambda (a)
                                (prog1
                                    (if (zerop a) 0 c)
                                  (setf c (ash (+ c a) 1))))
                              counts)))
         ;; range of bit sizes used
         (min (or (position-if-not 'zerop counts) 15))
         (max (or (position-if-not 'zerop counts :from-end t) 0))
         ;; temp space for sorting table
         (terminals (make-huffman-tree)))
    (declare (type (unsigned-byte 4) min max)
             (ignorable max)
             (type (simple-array (unsigned-byte 11) (16)) counts))
    (when (zerop max)
      (return-from build-tree-part (values 0 0)))
    ;; sort table/allocate codes
    (loop with offset-tmp = (copy-seq offsets)
          for i fixnum from 0
          for l across table
          for nodes of-type (simple-array (unsigned-byte 16) 1)
            = (ht-nodes terminals)
          for o = (aref offset-tmp l)
          for co = (aref code-offsets l)
          when (plusp l)
            do (incf (aref offset-tmp l))
               (cond
                 ((member type '(:dist :dht-len))
                  (setf (aref nodes o)
                        (if (<= i 29)
                            (ht-dist-node i)
                            ;; codes above 29 aren't used
                            (ht-invalid-node))))
                 ((> i +lengths-end+)
                  (setf (aref nodes o) (ht-invalid-node)))
                 ((>= i +lengths-start+)
                  (setf (aref nodes o) (ht-len-node i)))
                 ((= i +end-code+)
                  (setf (aref nodes o) (ht-end-node)))
                 (t
                  (setf (aref nodes o) (ht-literal-node i)))))

    ;; fill tree:
    (let ((next-subtable tree-offset))
      (labels ((next-len (l)
                 (position-if 'plusp counts :start l))
               (subtable (prefix prefix-bits)
                 (declare (ignorable prefix))
                 (or
                  (loop for entry-bits = (next-len prefix-bits)
                        while entry-bits
                        if (= prefix-bits entry-bits)
                          return (prog1 (aref (ht-nodes terminals)
                                              (aref offsets entry-bits))
                                   (incf (aref offsets entry-bits))
                                   (decf (aref counts entry-bits)))
                        else
                          return (let ((start next-subtable)
                                       (b  (- entry-bits prefix-bits)))
                                   (incf next-subtable (expt 2 b))
                                   (loop for i below (expt 2 b)
                                         do (setf (aref (ht-nodes tree)
                                                        (+ start (bit-rev i b)))
                                                  (subtable i entry-bits)))
                                   (values (ht-link-node b start))))
                  (ht-invalid-node))))
        (incf next-subtable (expt 2 min))
        (loop for i below (expt 2 min)
              do (setf (aref (ht-nodes tree)
                             (+ tree-offset (bit-rev i min)))
                       (subtable i min))))
      (values next-subtable min))))

(defun build-tree (tree lit/len dist)
  (declare (optimize speed))
  (multiple-value-bind (count bits)
      (build-tree-part tree lit/len 0 :lit/len)
    (setf (ht-len-start-bits tree) bits)
    (setf (ht-dist-offset tree) count)
    (setf (ht-dist-start-bits tree)
          (nth-value 1 (build-tree-part tree dist count :dist)))))

(defun dump-tree (tree &key bits base (depth 0))
  (cond
    ((and bits base)
     (loop for i below (expt 2 bits)
           for node = (aref (ht-nodes tree) (+ i base))
           do (format *debug-io* "~a~4,' d: ~a~%"
                      (make-string depth :initial-element #\~)
                      i
                      (ecase (ht-node-type node)
                        (#.+ht-literal+ (list :literal (ht-value node)))
                        (#.+ht-link/end+
                         (if (ht-endp node) :end
                             (list :link
                                   :bits (ht-link-bits node)
                                   :offset (ht-link-offset node))))
                        (#.+ht-len/dist+
                         (let ((v  (ht-value node)))
                           (list :len/dist v
                                 (when (> v +lengths-extra-bits-offset+)
                                   (+ v
                                      +lengths-start+
                                      (- +lengths-extra-bits-offset+)))
                                 :start (aref *len/dist-bases* v)
                                 :end (+ (aref *len/dist-bases* v)
                                         (1- (expt 2 (aref *extra-bits* v)))))))
                        (#.+ht-invalid+ :invalid)))
              (when (and (ht-linkp node)
                         (not (or (ht-endp node)
                                  (ht-invalidp node))))
                (dump-tree tree :bits (ht-link-bits node)
                                :base (ht-link-offset node)
                                :depth (+ depth 2)))))
    (t
     (format *debug-io* "lit/len table:~%")
     (dump-tree tree :bits (ht-len-start-bits tree)
                     :base 0 :depth 1)
     (format *debug-io* "distance table:~%")
     (when (plusp (ht-dist-start-bits tree))
       (dump-tree tree :bits (ht-dist-start-bits tree)
                       :base (ht-dist-offset tree)
                       :depth 1)))))

(Defparameter *static-huffman-tree* (make-huffman-tree))
(build-tree *static-huffman-tree* *fixed-lit/length-table* *fixed-dist-table*)
(dump-tree *static-huffman-tree*)

(defstruct (deflate-state (:conc-name ds-))
  ;; storage for dynamic huffman tree, modified for each dynamic block
  (dynamic-huffman-tree (make-huffman-tree) :type huffman-tree)
  ;; reference to either dynamic-huffman-tree or *static-huffman-tree*
  ;; depending on curret block
  (current-huffman-tree *static-huffman-tree* :type huffman-tree)
  ;; # of bits to read for current huffman tree level
  (tree-bits 0 :type ht-bit-count-type)
  ;; offset of current subtable in nodes array of current-huffman-tree
  (tree-offset 0 :type ht-offset-type)
  ;; # of extra bits being read in extra-bits state
  (extra-bits-needed 0 :type ht-bit-count-type)
  ;; last literal decoded from huffman tree
  ;; (last-decoded-literal 0 :type (unsigned-byte 8)) ;; unused?
  ;; last decoded length/distance value
  (last-decoded-len/dist 0 :type (unsigned-byte 16))
  ;; flag indicating if we are reading len or dist
  (reading-dist-flag nil :type (or nil t))
  ;; flag indicating we are reading the dynamic huffman table length table
  (reading-dht-flag nil :type (or nil t))
  ;; set when reading last block
  (last-block-flag nil :type (or nil t))
  ;; number of bytes left to copy (for uncompressed block, or from
  ;; history in compressed block)
  (bytes-to-copy 0 :type (unsigned-byte 16))
  ;; offset (from current output position) in history of source of
  ;; current copy in compressed block
  (history-copy-offset 0 :type (unsigned-byte 16))
  ;; current state machine state
  (current-state :start-of-block)
  ;; output ring-buffer (todo: generalize this)
  (output-buffer (make-array 65536 :element-type '(unsigned-byte 8)))
  (output-index 0 :type (unsigned-byte 16))
  ;; dynamic huffman tree parameters being read
  (dht-hlit 0 :type (unsigned-byte 10))
  (dht-hdist 0 :type octet)
  (dht-hclen 0 :type octet)
  (dht-len-code-index 0 :type octet)
  (dht-len-codes (make-array 20 :element-type 'octet :initial-element 0)
   :type octet-vector)
  (dht-len-tree (make-huffman-tree)) ;; fixme: reduce size
  (dht-lit/len (make-array 288 :element-type '(unsigned-byte 4)
                               :initial-element 0 :fill-pointer 0))
  (dht-dist (make-array 32 :element-type '(unsigned-byte 4)
                           :initial-element 0 :fill-pointer 0))
  (dht-last-len 0 :type octet)
;;; bitstream state:
  ;; # of bits remaining to read if read was interrupted
                                        ;(partial-bits-needed 0 :type (unsigned-byte 4))
  ;; bits read so far if read was interrupted (largest value we read
  ;; at once is 16bit size, so fi)
                                        ;(partial-bits-read 0 :type (unsigned-byte 16))
  ;; remaining bits of last partially consumed octet (or
  (partial-bits 0 :type (unsigned-byte 16))
  ;; # of valid bits remaining in partial-bits (0 = none)
  (bits-remaining 0 :type (unsigned-byte 4)))

(defmacro with-bit-readers ((state) &body body)
  `(macrolet (;; true if previous read was interrupted and we
              ;; can't make any more progress without new input
              #++(blockedp ()
                   `(and (plusp (ds-partial-bits-needed ,',state))
                         (zerop (octets-left))))
              ;; consume any leftover bits from last read
              (%use-partial-bits (n)
                `(progn
                   #++(format *debug-io* "use partial bits ~s/~s = ~8,'0b~%"
                              ,n (ds-bits-remaining ,',state)
                              (ds-partial-bits ,',state))
                   (prog1
                       (ldb (byte ,n 0) (ds-partial-bits ,',state))
                     (decf (ds-bits-remaining ,',state) ,n)
                     (setf (ds-partial-bits ,',state)
                           (ash (ds-partial-bits ,',state) (- ,n))))))
              ;; try to get more bits from source (only called when
              ;; there aren't enough already read)
              (%try-read-bits (bits interrupt-form)
                (with-gensyms (tmp n o r octet)
                  `(let ((,tmp 0)
                         (,n ,bits)
                         (,o 0))
                     (declare (type (unsigned-byte 5) ,n ,o)
                              (type (unsigned-byte 16) ,tmp))
                     #++(format *debug-io* "try-read-bits ~s/~s = ~16,'0b~%"
                                ,n
                                (ds-bits-remaining ,',state)
                                (ds-partial-bits ,',state))
                     (flet ((int ()
                              ;; if we ran out of input, store what we
                              ;; have so we can try again later
                              (setf (ds-bits-remaining ,',state) ,o
                                    (ds-partial-bits ,',state) ,tmp)
                              ;; and let caller decide what to do next
                              ,interrupt-form))
                       ;; we had some partial input, so start with that
                       (when (plusp (ds-bits-remaining ,',state))
                         #++(format *debug-io* " used remaining ~s ~8,'0b~%"
                                    (ds-bits-remaining ,',state)
                                    (ds-partial-bits ,',state))
                         (setf ,tmp (ds-partial-bits ,',state))
                         (setf ,o (ds-bits-remaining ,',state)))
                       ;; we only support up to 16bit values, so just
                       ;; hard code 2 tries to read more bits
                       (let ((,octet 0)
                             (,r (- ,n ,o)))
                         (declare (type (unsigned-byte 5) ,r)
                                  (type octet ,octet))
                         ;; we need at least 1 octet, so try to
                         ;; consume it directly
                         (when (>= ,r 8)
                           (setf ,octet (octet (int)))
                           (setf (ldb (byte 8 ,o) ,tmp) ,octet)
                           (incf ,o 8)
                           (decf ,r 8))
                         ;; we need at least 1 bit and less than an
                         ;; octet (or we tried to read more than 16
                         ;; bits), so try to consume part of an octet
                         (when (plusp ,r)
                           (setf ,octet (octet (int)))
                           (setf (ldb (byte ,r ,o) ,tmp) ,octet))
                         (if (or (zerop ,r) (= ,r 8))
                             ;; used up entire octet
                             (setf (ds-bits-remaining ,',state) 0)
                             ;; used up partial octet
                             (setf (ds-bits-remaining ,',state) (- 8 ,r)
                                   (ds-partial-bits ,',state)
                                   (ash ,octet (- ,r))))))
                     #++(format *debug-io* " -> ~4,'0x ~16,'0b~%" ,tmp ,tmp)
                     ,tmp)))
              (bits (n interrupt-form)
                `(progn
                   (assert (plusp ,n))
                   (if (>= (ds-bits-remaining ,',state) ,n)
                       (%use-partial-bits ,n)
                       (%try-read-bits ,n ,interrupt-form))))
              (byte-align ()
                `(setf (ds-bits-remaining ,',state) 0)))
     ,@body))




(defmacro state-machine ((state) &body tagbody)
  (with-gensyms (next-state)
    (let ((tags (loop for form in tagbody when (atom form) collect form)))
      `(macrolet ((next-state (,next-state)
                    `(progn
                       #++(setf (ds-current-state ,',state) ',,next-state)
                       (go ,,next-state))))
         (tagbody
            ;; possibly could do better than a linear search here, but
            ;; if state machine is being interrupted often enough to
            ;; matter, it probably won't matter anyway :/ at most,
            ;; maybe define more commonly interrupted states earlier
            (ecase (ds-current-state ,state)
              ,@(loop for i in tags
                      collect `(,i (go ,i))))
            ,@(loop for f in tagbody
                    collect f
                    when (atom f)
                      ;;collect `(format *debug-io* "=>~s~%" ',f) and
                      collect `(setf (ds-current-state ,state) ',f)))))))


(declaim (type (simple-array octet (19)) *len-code-order*))
(defparameter *len-code-order*
  (coerce #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
          '(simple-array (unsigned-byte 8) (19))))

(defun decompress (read-context state &key into)
  (declare (type (or null octet-vector) into))
  (with-reader-contexts (read-context)
    (with-bit-readers (state)
      (let ((output (ds-output-buffer state))
            (out nil)
            (count 0))
        (declare (type (simple-array (unsigned-byte 8) (65536)) output)
                 (fixnum count))
        (labels ((copy-output (from to)
                   (if into
                       (replace into output
                                :start1 count
                                :start2 from :end2 to)
                       (push (subseq output from to) out))
                   (incf count (- to from)))
                 (out-byte (x)
                   #++(format t " ~2,'0x" x)
                   #++(if (<= 32 x 127)
                          (format t " {~c}" (code-char x))
                          (format t " <~d>" x))
                   #++(format *debug-io* "##out ~2,'0x~%" x)
                   (setf (aref output (ds-output-index state)) x)
                   (setf (ds-output-index state)
                         (ldb (byte 16 0) (1+ (ds-output-index state))))
                   (when (zerop (ds-output-index state))
                     (copy-output 32768 65536))
                   (when (= 32768 (ds-output-index state))
                     (copy-output 0 32768)))
                 (copy-history-byte (offset)
                   (declare (type (unsigned-byte 16) offset))
                   #++(format *debug-io* "copy history ~s~%" offset)
                   (let ((x (aref output (ldb (byte 16 0)
                                              (+ 65536
                                                 (- offset)
                                                 (ds-output-index state))))))
                     (out-byte x)))
                 (store-dht (v)
                   (cond
                     ((plusp (ds-dht-hlit state))
                      #++(format *debug-io* "$$lit/len ~s (~s)~%"
                                 v (ds-dht-hlit state))
                      (vector-push v (ds-dht-lit/len state))
                      (decf (ds-dht-hlit state)))
                     ((plusp (ds-dht-hdist state))
                      #++(format *debug-io* "$$dist ~s (~s)~%"
                                 v (ds-dht-hdist state))
                      (vector-push v (ds-dht-dist state))
                      (decf (ds-dht-hdist state)))
                     (t
                      (error "???"))))
                 (repeat-dht (c)
                   (loop repeat c do (store-dht (ds-dht-last-len state)))))
          (declare (ignorable #'copy-history-byte)
                   (inline out-byte store-dht))

          (state-machine (state)
            :start-of-block
            (let ((final (the bit (bits 1 (error "foo!"))))
                  (type (the (unsigned-byte 2) (bits 2 (error "foo2")))))
              #++(format t "block ~s ~s~%" final type)
              (setf (ds-last-block-flag state) (plusp final))
              (ecase type
                (0 (next-state :uncompressed-block))
                (1 (next-state :static-huffman-block))
                (2 (next-state :dynamic-huffman-block))))

            :uncompressed-block
            (byte-align)
            (let ((s (bits 16 (error "foo3")))
                  (n (the (unsigned-byte 16) (bits 16 (error "foo4")))))
              (assert (= n (ldb (byte 16 0) (lognot s))))
              (format *debug-io* "uncompressed ~s (~s)~%" s n)
              (loop repeat s
                    do (out-byte (the octet (bits 8 (error "5"))))))
            (next-state :block-end)

            :static-huffman-block
            (setf (ds-current-huffman-tree state)
                  *static-huffman-tree*)
            (next-state :decompress-block)

            :dynamic-huffman-block
            (setf (ds-dht-hlit state) (+ 257 (bits 5 (error "dhb1"))))
            (setf (fill-pointer (ds-dht-lit/len state)) 0)
            :dynamic-huffman-block2
            (setf (ds-dht-hdist state) (+ 1 (bits 5 (error "dhb2"))))
            (setf (fill-pointer (ds-dht-dist state)) 0)
            :dynamic-huffman-block3
            (setf (ds-dht-hclen state) (+ 4 (bits 4 (error "dhb3"))))
            (fill (ds-dht-len-codes state) 0)
            (setf (ds-dht-len-code-index state) 0)
            :dynamic-huffman-block-len-codes
            (loop while (plusp (ds-dht-hclen state))
                  for i = (aref *len-code-order* (ds-dht-len-code-index state))
                  do (setf (aref (ds-dht-len-codes state) i)
                           (bits 3 (error "dhb-lc")))
                     (incf (ds-dht-len-code-index state))
                     (decf (ds-dht-hclen state)))
            (setf (ht-len-start-bits (ds-dht-len-tree state))
                  (nth-value 1
                             (build-tree-part (ds-dht-len-tree state)
                                              (ds-dht-len-codes state)
                                              0 :dht-len)))
            #++(format *debug-io*
                       "build dht len tree: ~s~%"  (ds-dht-len-codes state))
            #++(dump-tree (ds-dht-len-tree state))
            (setf (ds-current-huffman-tree state)
                  (ds-dht-len-tree state))
            (setf (ds-reading-dht-flag state) t)
            (next-state :decode-huffman-entry)

            :dynamic-huffman-block4
            (build-tree (ds-dynamic-huffman-tree state)
                        (ds-dht-lit/len state)
                        (ds-dht-dist state))
            (setf (ds-reading-dht-flag state) nil)
            (setf (ds-current-huffman-tree state)
                  (ds-dynamic-huffman-tree state))
            (next-state :decompress-block)

            :block-end
            (if (ds-last-block-flag state)
                (next-state :done)
                (next-state :start-of-block))

            :decompress-block
            ;; reading a literal/length/end
            (next-state :decode-huffman-entry)


            :decode-huffman-entry
            (setf (ds-tree-bits state)
                  (ht-len-start-bits (ds-current-huffman-tree state)))
            (setf (ds-tree-offset state) 0)
            :decode-huffman-entry2
            ;; fixme: build table reversed instead of reversing bits
            (let* ((bits  (bits (ds-tree-bits state) (error "6")))
                   (node (aref (ht-nodes (ds-current-huffman-tree state))
                               (+ bits (ds-tree-offset state)))))
              #++(format *debug-io* "got ~d bits ~16,'0b -> node ~16,'0b~%"
                         (ds-tree-bits state)
                         bits node)
              (ecase (ht-node-type node)
                (#.+ht-literal+
                 (out-byte (ht-value node))
                 (next-state :decode-huffman-entry))
                (#.+ht-link/end+
                 (when (ht-endp node)
                   (next-state :block-end))
                 (setf (ds-tree-bits state) (ht-link-bits node)
                       (ds-tree-offset state) (ht-link-offset node))
                 (next-state :decode-huffman-entry2))
                (#.+ht-len/dist+
                 (if (ds-reading-dht-flag state)
                     ;; reading dynamic table lengths
                     (let ((v (ht-value node)))
                       (cond
                         ((< v 16)
                          (setf (ds-dht-last-len state) v)
                          (store-dht v)
                          (next-state :more-dht?))
                         (t
                          (setf (ds-extra-bits-needed state)
                                (ecase v (16 2) (17 3) (18 7)))
                          (setf (ds-last-decoded-len/dist state)
                                (ecase v (16 3) (17 3) (18 11)))
                          (when (> v 16)
                            (setf (ds-dht-last-len state) 0))
                          (next-state :extra-bits))))
                     ;; reading length or distance, with possible extra bits
                     (let ((v (ht-value node)))
                       (setf (ds-extra-bits-needed state)
                             (aref *extra-bits* v))
                       (setf (ds-last-decoded-len/dist state)
                             (aref *len/dist-bases* v))
                       #++(format *debug-io* " read l/d ~s: ~s ~s ~%"
                                  v
                                  (ds-extra-bits-needed state)
                                  (ds-last-decoded-len/dist state))
                       (next-state :extra-bits))))))
            :more-dht?
            #++(format *debug-io* "  ~s ~s~%"
                       (ds-dht-hlit state)
                       (ds-dht-hdist state))
            (if (or (plusp (ds-dht-hlit state))
                    (plusp (ds-dht-hdist state)))
                (next-state :decode-huffman-entry)
                (next-state :dynamic-huffman-block4))
            :extra-bits
            (when (plusp (ds-extra-bits-needed state))
              (let ((bits (bits (ds-extra-bits-needed state) (error "7"))))
                (declare (type (unsigned-byte 16) bits))
                #++(format *debug-io* " ~s extra bits = ~s~%"
                           (ds-extra-bits-needed state) bits)
                (incf (ds-last-decoded-len/dist state)
                      bits)))
            (cond
              ((ds-reading-dht-flag state)
               (repeat-dht (ds-last-decoded-len/dist state))
               (next-state :more-dht?))
              ((ds-reading-dist-flag state)
               (setf (ds-history-copy-offset state)
                     (ds-last-decoded-len/dist state))
               (setf (ds-reading-dist-flag state) nil)

               #++(format t "match ~s ~s~%"
                          (ds-bytes-to-copy state)
                          (ds-history-copy-offset state))
               (next-state :copy-history))
              (t
               (setf (ds-bytes-to-copy state)
                     (ds-last-decoded-len/dist state))
               (next-state :read-dist)))

            :read-dist
            (setf (ds-tree-bits state)
                  (ht-dist-start-bits (ds-current-huffman-tree state)))
            (setf (ds-tree-offset state)
                  (ht-dist-offset (ds-current-huffman-tree state)))
            (setf (ds-reading-dist-flag state) t)
            (next-state :decode-huffman-entry2)

            :copy-history
            (loop repeat (ds-bytes-to-copy state)
                  do (copy-history-byte (ds-history-copy-offset state)))
            (next-state :decompress-block)

            :done)
          (cond
            ((< 0 (ds-output-index state) 32768)
             (copy-output 0 (ds-output-index state)))
            ((< 32768 (ds-output-index state))
             (copy-output 32768 (ds-output-index state))))
          (if into
              into
              (reverse out)))))))


