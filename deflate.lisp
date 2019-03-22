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

(defmacro wrap-fixnum (x)
  ;; a few places we already checked something will be a fixnum (for
  ;; example an array index in a loop), so tell the compiler it doesn't
  ;; need to check for bignums
  `(ldb (byte #. (integer-length most-positive-fixnum) 0) ,x))

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
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (huffman-tree (:conc-name ht-))
    (len-start-bits 0 :type ht-bit-count-type)
    (dist-start-bits 0 :type ht-bit-count-type)
    (dist-offset 0 :type ht-offset-type)
    (nodes (make-array +max-tree-size+ :element-type 'ht-node-type
                                       :initial-element (ht-invalid-node))
     :type ht-node-array-type)))

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

(alexandria:define-constant +extra-bits+
    (concatenate
     '(simple-array (unsigned-byte 4) (61))
     (replace (make-array +lengths-extra-bits-offset+ :initial-element 0)
              #(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
     #(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
  :test 'equalp)


;; base length value for each length/distance code, add to extra bits
;; to get length
(alexandria:define-constant +len/dist-bases+
    (concatenate '(simple-array (unsigned-byte 16) (61))
                 (replace (make-array +lengths-extra-bits-offset+ :initial-element 0)
                          #(1 2 3 4 5 7 9 13 17 25 33 49 65 97
                            129 193 257 385 513 769
                            1025 1537 2049 3073 4097 6145 8193
                            12289 16385 24577))
                 #(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99
                   115 131 163 195 227 258))
  :test 'equalp)

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

(defun build-tree-part (tree tree-offset table type start end
                        scratch)
  (declare (type fixnum tree-offset start end)
           (type code-table-type table)
           (optimize speed))
  (assert (typep scratch 'huffman-tree))
  ;; # of entries of each bit size
  (let* ((counts (let ((a (make-array 16 :element-type '(unsigned-byte 11)
                                         :initial-element 0)))
                   (loop for x from start below end
                         for i = (aref table x)
                         when (plusp i) do (incf (aref a i)))
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
         (min (position-if-not 'zerop counts))
         ;; temp space for sorting table
         (terminals scratch))
    (declare (type (or null (unsigned-byte 4)) min)
             (type (simple-array (unsigned-byte 11) (16)) counts)
             (dynamic-extent counts offsets code-offsets))
    (unless min
      (return-from build-tree-part (values 0 0)))
    ;; sort table/allocate codes
    (loop with offset-tmp = (copy-seq offsets)
          for i fixnum from 0
          for to fixnum from start below end
          for l = (aref table to)
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
      (declare (type (unsigned-byte 12) next-subtable))
      (labels ((next-len (l)
                 (position-if #'plusp counts :start l))
               (subtable (prefix prefix-bits)
                 (declare (ignorable prefix))
                 (or
                  (loop for entry-bits = (if (zerop (aref counts prefix-bits))
                                             (next-len prefix-bits)
                                             prefix-bits)
                        while entry-bits
                        if (= prefix-bits entry-bits)
                          return (prog1 (aref (ht-nodes terminals)
                                              (aref offsets entry-bits))
                                   (incf (aref offsets entry-bits))
                                   (decf (aref counts entry-bits)))
                        else
                          return (let ((start next-subtable)
                                       (b  (- entry-bits prefix-bits)))
                                   (declare (type (unsigned-byte 16) b))
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
  (declare (optimize speed)
           (type code-table-type lit/len dist))
  (multiple-value-bind (count bits)
      (build-tree-part tree 0 lit/len :lit/len 0 (length lit/len)
                       (make-huffman-tree))
    (setf (ht-len-start-bits tree) bits)
    (setf (ht-dist-offset tree) count)
    (setf (ht-dist-start-bits tree)
          (nth-value 1 (build-tree-part tree count dist :dist
                                        0 (length dist)
                                        (make-huffman-tree))))))

(defun build-tree* (tree lit/len/dist mid end scratch)
  (declare (optimize speed)
           (type (vector (unsigned-byte 4)) lit/len/dist)
           (type (and unsigned-byte fixnum) mid))
  (multiple-value-bind (count bits)
      (build-tree-part tree 0 lit/len/dist :lit/len 0 mid scratch)
    (setf (ht-len-start-bits tree) bits)
    (setf (ht-dist-offset tree) count)
    (setf (ht-dist-start-bits tree)
          (nth-value 1 (build-tree-part tree count
                                        lit/len/dist :dist
                                        mid end
                                        scratch)))
    #++(dump-tree tree)))

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
                                 :start (aref +len/dist-bases+ v)
                                 :end (+ (aref +len/dist-bases+ v)
                                         (1- (expt 2 (aref +extra-bits+ v)))))))
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

(defconstant +static-huffman-tree+ (if (boundp '+static-huffman-tree+)
                                       +static-huffman-tree+
                                       (make-huffman-tree)))

(build-tree +static-huffman-tree+ *fixed-lit/length-table* *fixed-dist-table*)
(dump-tree +static-huffman-tree+)

(defstruct (deflate-state (:conc-name ds-))
  ;; storage for dynamic huffman tree, modified for each dynamic block
  (dynamic-huffman-tree (make-huffman-tree) :type huffman-tree)
  ;; reference to either dynamic-huffman-tree or *static-huffman-tree*
  ;; depending on curret block
  (current-huffman-tree +static-huffman-tree+ :type huffman-tree)
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
  ;; indicate consumer of extra bits
  (extra-bits-type 0 :type (unsigned-byte 2))
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
  (dht-hlit 0 :type  (unsigned-byte 10))
  (dht-hlit+hdist 0 :type (unsigned-byte 10))
  (dht-hclen 0 :type octet)
  (dht-len-code-index 0 :type octet)
  (dht-len-codes (make-array 20 :element-type '(unsigned-byte 4)
                                :initial-element 0)
   :type code-table-type)
  (dht-len-tree (make-huffman-tree)) ;; fixme: reduce size
  (dht-lit/len/dist (make-array (+ 288 32) :element-type '(unsigned-byte 4)
                                           :initial-element 0)
   :type code-table-type)
  (dht-lit/len/dist-index 0 :type (mod 320))
  (dht-last-len 0 :type octet)
  ;; bitstream state: we read up to 64bits at a time to try to
  ;; minimize time spent interacting with input stream relative to
  ;; decoding time.
  (partial-bits 0 :type (unsigned-byte 64))
  ;; # of valid bits remaining in partial-bits (0 = none)
  ;; (bits-remaining 0 :type (unsigned-byte 6)) bit offset of
  ;; remaining valid bits in partial-bits. 64 = none remaining.  note
  ;; that when storing less than 64 bits (at end of input, etc), we
  ;; need to use upper bits
  (partial-bit-offset 64 :type (unsigned-byte 7)))

(defmacro with-bit-readers ((state) &body body)
  `(let ((partial-bit-offset (ds-partial-bit-offset ,state))
         (partial-bits (ds-partial-bits state)))
     (declare (type (unsigned-byte 7) partial-bit-offset)
              (type (unsigned-byte 64) partial-bits))
     (macrolet (;; use cached bits (only called when we can fill current
                ;; read from cache)
                (%use-partial-bits (n n2)
                  `(prog1
                       (ldb (byte ,n partial-bit-offset)
                            partial-bits)
                     (setf partial-bit-offset  ,n2)))
                ;; try to get more bits from source (only called when
                ;; there aren't enough already read)
                (%try-read-bits (n interrupt-form)
                  `(%%try-read-bits ,n (lambda () ,interrupt-form)))
                (bits (n interrupt-form)
                  (with-gensyms (n2)
                    (once-only (n)
                      `(let ((,n2 (+ partial-bit-offset
                                     (the (unsigned-byte 6) ,n))))
                         (if (<= ,n2 64)
                             (%use-partial-bits ,n ,n2)
                             (%try-read-bits ,n ,interrupt-form))))))
                (byte-align ()
                  `(setf partial-bit-offset
                         (* 8 (ceiling partial-bit-offset  8)))))
       (flet ((%%try-read-bits (n interrupt-form)
                (let ((tmp 0)
                      (o 0))
                  (declare (type (unsigned-byte 5) o)
                           (type (unsigned-byte 16) tmp)
                           (type (unsigned-byte 6) n))
                  (flet ((int ()
                           ;; if we ran out of input, store what we
                           ;; have so we can try again later
                           (assert (< o 16))
                           (let ((r (- 64 o)))
                             (declare (type (unsigned-byte 6) r)
                                      (type (unsigned-byte 4) o))
                             (setf partial-bit-offset r)
                             (setf partial-bits
                                   (ldb (byte 64 0)
                                        (ash (ldb (byte o 0) tmp) r))))
                           ;; and let caller decide what to do next
                           (funcall interrupt-form)
                           0))
                    ;; we had some leftover bits, try to use them
                    ;; before getting more input
                    (when (< partial-bit-offset 64)
                      (let ((r (- 64 partial-bit-offset)))
                        (assert (<= r 16))
                        (setf tmp
                              (ldb (byte r partial-bit-offset)
                                   partial-bits))
                        (setf o r)))
                    ;; try to read more bits from input
                    (multiple-value-bind (input octets)
                        (word64)
                      (cond
                        ((= octets 8)
                         (setf partial-bit-offset 0
                               partial-bits input))
                        ((zerop octets)
                         (setf partial-bit-offset 64))
                        (t
                         (let ((r (* 8 (- 8 octets))))
                           (declare (type (unsigned-byte 6) r))
                           (setf partial-bit-offset r)
                           (setf partial-bits
                                 (ldb (byte 64 0) (ash input r))))))
                      (when (and (zerop o)
                                 (< (+ partial-bit-offset n) 64))
                        (return-from %%try-read-bits
                          (%use-partial-bits n (+ partial-bit-offset n)))))
                    ;; consume any additional available input
                    (let* ((n2 (- n o))
                           (n3 (+ partial-bit-offset n2)))
                      (declare (type (unsigned-byte 4) n2)
                               (type (unsigned-byte 7) n3))
                      (cond
                        ;; we have enough bits to finish read
                        ((< n3 64)
                         (setf (ldb (byte n2 o) tmp)
                               (ldb (byte n2 partial-bit-offset)
                                    partial-bits))
                         (setf partial-bit-offset n3))
                        ;; we have some bits, but not enough. consume
                        ;; what is available and error
                        ((< partial-bit-offset 64)
                         (let ((r (- 64 partial-bit-offset)))
                           (declare (type (unsigned-byte 4) r ))
                           (setf (ldb (byte r o) tmp)
                                 (ldb (byte r partial-bit-offset)
                                      partial-bits))
                           (incf o r)
                           (int)))
                        ;; didn't get any new bits, error
                        (t
                         (int)))))
                  ;; if we got here, return results
                  tmp)))
         (declare (notinline %%try-read-bits))
         ,@body))))


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
(alexandria:define-constant +len-code-order+
    (coerce #(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15)
            '(simple-array (unsigned-byte 8) (19)))
  :test 'equalp)

(defparameter *stats* (make-hash-table))
(defun decompress (read-context state &key into)
  (declare (type (or null octet-vector) into)
           (optimize speed))
  (check-type into octet-vector)
  (assert into)
  (with-reader-contexts (read-context)
    (with-bit-readers (state)
      (let ((count 0)
            (copy-offset (ds-history-copy-offset state))
            (bytes-to-copy (ds-bytes-to-copy state))
            (dht-hlit (ds-dht-hlit state))
            (dht-hlit+hdist (ds-dht-hlit+hdist state))
            (dht-hclen (ds-dht-hclen state))
            (dht-lit/len/dist (ds-dht-lit/len/dist state))
            (dht-lit/len/dist-index (ds-dht-lit/len/dist-index state))
            (dht-len-codes (ds-dht-len-codes state))
            (dht-len-code-index (ds-dht-len-code-index state))
            (dht-last-len (ds-dht-last-len state))
            (current-huffman-tree (ds-current-huffman-tree state))
            (ht-nodes (ht-nodes (ds-current-huffman-tree state)))
            (dht-len-tree (ds-dht-len-tree state))
            (extra-bits-type (ds-extra-bits-type state))
            (tree-bits (ds-tree-bits state))
            (tree-offset (ds-tree-offset state))
            (extra-bits-needed (ds-extra-bits-needed state))
            (last-decoded-len/dist (ds-last-decoded-len/dist state))
            (last-block-flag (ds-last-block-flag state))
            (ht-scratch (make-huffman-tree)))
        (declare (type (simple-array (unsigned-byte 8) 1) into)
                 (fixnum count)
                 (type (unsigned-byte 15) copy-offset)
                 (type (unsigned-byte 10) dht-hlit dht-hlit+hdist)
                 (type (simple-array (unsigned-byte 4) (320)) dht-lit/len/dist)
                 (type (mod 320) dht-lit/len/dist-index)
                 (type octet dht-hclen dht-last-len)
                 (type (simple-array (unsigned-byte 4) (20)) dht-len-codes)
                 (type (mod 20) dht-len-code-index)
                 (type huffman-tree
                       current-huffman-tree dht-len-tree ht-scratch)
                 (type (simple-array ht-node-type 1) ht-nodes)
                 (type (mod 3) extra-bits-type)
                 (type (unsigned-byte 4) tree-bits extra-bits-needed)
                 (type (unsigned-byte 11) tree-offset)
                 (type (unsigned-byte 16) last-decoded-len/dist bytes-to-copy)
                 (type (or t nil) last-block-flag))
        (labels ((out-byte (x)
                   (setf (aref into count) x)
                   (incf count))
                 (copy-history (n)
                   (declare (type fixnum n))
                   (let ((o copy-offset))
                     (let* ((d count)
                            (n1 n)
                            (s (- d o))
                            (e (length into)))
                       (declare (type fixnum d s e))
                       ;; if copy won't fit, use slow path for now
                       (when (> (+ d n) e)
                         (loop while (< d e)
                               do (setf (aref into d) (aref into s))
                                  (setf d (1+ d))
                                  (setf s (1+ s)))
                         ;; todo: store state so it can continue
                         (error "output full"))
                       ;; to speed things up, we allow writing past
                       ;; current output index (but not past end of
                       ;; buffer), and read/write as many bytes at a
                       ;; time as possible.
                       (cond
                         ((>= o 8)
                          (loop repeat (ceiling n 8)
                                do (setf (nibbles:ub64ref/le into d)
                                         (nibbles:ub64ref/le into s))
                                   (setf d (wrap-fixnum (+ d 8)))
                                   (setf s (wrap-fixnum (+ s 8)))))
                         ((>= o 4)
                          (loop repeat (ceiling n 4)
                                do (setf (nibbles:ub32ref/le into d)
                                         (nibbles:ub32ref/le into s))
                                   (setf d (wrap-fixnum (+ d 4)))
                                   (setf s (wrap-fixnum (+ s 4)))))
                         ((>= o 2)
                          (loop repeat (ceiling n 2)
                                do (setf (nibbles:ub16ref/le into d)
                                         (nibbles:ub16ref/le into s))
                                   (setf d (wrap-fixnum (+ d 2)))
                                   (setf s (wrap-fixnum (+ s 2)))))
                         (t
                          ;; if offset is 1, we are just repeating a
                          ;; single byte...
                          (loop with x of-type octet = (aref into s)
                                repeat n
                                do (setf (aref into d) x)
                                   (setf d (wrap-fixnum (1+ d))))))
                       ;; D may be a bit past actual value, so calculate
                       ;; correct offset
                       (setf count (+ count n1)))))
                 (store-dht (v)
                   (cond
                     ((plusp dht-hlit+hdist)
                      (setf (aref dht-lit/len/dist dht-lit/len/dist-index)
                            v)
                      (incf dht-lit/len/dist-index)
                      (decf dht-hlit+hdist))
                     (t
                      (error "???"))))
                 (repeat-dht (c)
                   (loop repeat c do (store-dht dht-last-len))
                   #++
                   (progn
                     (fill (ds-dht-lit/len/dist state)
                           (ldb (byte 4 0) (ds-dht-last-len state))
                           :start (ds-dht-lit/len/dist-index state)
                           :end (+ c (ds-dht-lit/len/dist-index state)))
                     (incf (ds-dht-lit/len/dist-index state) c)
                     (decf (ds-dht-hlit+hdist state) c))))
          (declare (inline out-byte store-dht))

          (state-machine (state)
            :start-of-block
            (let ((final (the bit (bits 1 (error "foo!"))))
                  (type (the (unsigned-byte 2) (bits 2 (error "foo2")))))
              #++(format t "block ~s ~s~%" final type)
              (setf last-block-flag (plusp final))
              (ecase type
                (0 (next-state :uncompressed-block))
                (1 (next-state :static-huffman-block))
                (2 (next-state :dynamic-huffman-block))))

            :uncompressed-block
            (byte-align)
            (let ((s (bits 16 (error "foo3")))
                  (n (the (unsigned-byte 16) (bits 16 (error "foo4")))))
              (assert (= n (ldb (byte 16 0) (lognot s))))
              #++(format *debug-io* "uncompressed ~s (~s)~%" s n)
              (loop repeat s
                    do (out-byte (the octet (bits 8 (error "5"))))))
            (next-state :block-end)

            :static-huffman-block
            (setf current-huffman-tree +static-huffman-tree+)
            (setf ht-nodes (ht-nodes current-huffman-tree))
            (next-state :decompress-block)

            :dynamic-huffman-block
            (setf dht-hlit (+ 257 (bits 5 (error "dhb1"))))
            #++(format t "hlit = ~s~%" (ds-dht-hlit state))
            :dynamic-huffman-block2
            (let ((hdist (+ 1 (bits 5 (error "dhb2")))))
              (setf dht-hlit+hdist (+ dht-hlit hdist)))
            (setf dht-lit/len/dist-index 0)
            :dynamic-huffman-block3
            (setf dht-hclen (+ 4 (bits 4 (error "dhb3"))))
            (fill dht-len-codes 0)
            (setf dht-len-code-index 0)
            :dynamic-huffman-block-len-codes
            (loop while (plusp dht-hclen)
                  for i = (aref +len-code-order+ dht-len-code-index)
                  do (setf (aref dht-len-codes i)
                           (bits 3 (error "dhb-lc")))
                     (incf dht-len-code-index)
                     (decf dht-hclen))
            (setf (ht-len-start-bits dht-len-tree)
                  (nth-value 1
                             (build-tree-part dht-len-tree 0
                                              dht-len-codes
                                              :dht-len 0 20
                                              ht-scratch)))
            #++(format *debug-io*
                       "build dht len tree: ~s~%"  (ds-dht-len-codes state))
            #++(dump-tree (ds-dht-len-tree state))
            (setf current-huffman-tree dht-len-tree)
            (setf ht-nodes (ht-nodes current-huffman-tree))
            (setf extra-bits-type 2)
            (next-state :decode-huffman-entry)

            :dynamic-huffman-block4
            (build-tree* (ds-dynamic-huffman-tree state)
                         dht-lit/len/dist
                         dht-hlit
                         dht-lit/len/dist-index
                         ht-scratch)
            (setf extra-bits-type 0)
            (setf current-huffman-tree (ds-dynamic-huffman-tree state))
            (setf ht-nodes (ht-nodes current-huffman-tree))
            (next-state :decompress-block)

            :block-end
            (if last-block-flag
                (next-state :done)
                (next-state :start-of-block))

            :decompress-block
            ;; reading a literal/length/end
            (next-state :decode-huffman-entry)


            :decode-huffman-entry
            (setf tree-bits
                  (ht-len-start-bits current-huffman-tree))
            (setf tree-offset 0)
            :decode-huffman-entry2
            (let* ((bits (bits tree-bits (error "6")))
                   (node (aref ht-nodes (+ bits tree-offset))))
              #++(format *debug-io* "got ~d bits ~16,'0b -> node ~16,'0b~%"
                         (ds-tree-bits state)
                         bits node)
              ;; test file shows ~ 1.5:1.3:0.5 for link:len/dist:literal
              (ecase (ht-node-type node)
                (#.+ht-link/end+
                 (when (ht-endp node)
                   (next-state :block-end))
                 (setf tree-bits (ht-link-bits node)
                       tree-offset (ht-link-offset node))
                 (next-state :decode-huffman-entry2))
                (#.+ht-len/dist+
                 (if (= extra-bits-type 2)
                     ;; reading dynamic table lengths
                     (let ((v (ht-value node)))
                       #++(incf (gethash v *stats* 0))
                       (cond
                         ((< v 16)
                          (setf dht-last-len v)
                          (store-dht v)
                          (next-state :more-dht?))
                         ((= v 16)
                          (setf extra-bits-needed 2
                                last-decoded-len/dist 3))
                         ((= v 17)
                          (setf extra-bits-needed 3
                                last-decoded-len/dist 3
                                dht-last-len 0))
                         (t
                          (setf extra-bits-needed 7
                                last-decoded-len/dist 11
                                dht-last-len 0)))
                       (next-state :extra-bits)))
                 ;; reading length or distance, with possible extra bits
                 (let ((v (ht-value node)))
                   (setf extra-bits-needed
                         (aref +extra-bits+ v))
                   (setf last-decoded-len/dist
                         (aref +len/dist-bases+ v))
                   #++(format *debug-io* " read l/d ~s: ~s ~s ~%"
                              v
                              (ds-extra-bits-needed state)
                              (ds-last-decoded-len/dist state))
                   (next-state :extra-bits)))
                (#.+ht-literal+
                 (out-byte (ht-value node))
                 (next-state :decode-huffman-entry))))
            :more-dht?
            #++(format *debug-io* "  ~s ~s~%"
                       (ds-dht-hlit state)
                       (ds-dht-hdist state))
            (if (plusp dht-hlit+hdist)
                (next-state :decode-huffman-entry)
                (progn
                  (setf extra-bits-type 0)
                  (next-state :dynamic-huffman-block4)))
            :extra-bits
            (when (plusp extra-bits-needed)
              (let ((bits (bits extra-bits-needed (error "7"))))
                (declare (type (unsigned-byte 16) bits))
                #++(format *debug-io* " ~s extra bits = ~s~%"
                           (ds-extra-bits-needed state) bits)
                (incf last-decoded-len/dist bits)))
            (ecase extra-bits-type
              (0 ;; len
               (setf bytes-to-copy last-decoded-len/dist)
               (next-state :read-dist))
              (1 ;; dist
               (setf copy-offset last-decoded-len/dist)
               (setf extra-bits-type 0)
               #++(format t "match ~s ~s~%"
                          (ds-bytes-to-copy state)
                          (ds-history-copy-offset state))
               (next-state :copy-history))
              (2 ;; dht
               (repeat-dht last-decoded-len/dist)
               (next-state :more-dht?)))

            :read-dist
            (setf tree-bits (ht-dist-start-bits current-huffman-tree))
            (setf tree-offset (ht-dist-offset current-huffman-tree))
            (setf extra-bits-type 1)
            (next-state :decode-huffman-entry2)

            :copy-history
            (copy-history bytes-to-copy)
            (next-state :decompress-block)

            :done)
          into)))))
