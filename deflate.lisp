(in-package 3bz)

#++(ql:quickload '3bz)
(defstruct-cached (deflate-state (:conc-name ds-))
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

(defmacro with-bit-readers ((state ) &body body)
  `(macrolet (;; use cached bits (only called when we can fill current
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
       ,@body)))


(defmacro state-machine ((state) &body tagbody)
  (with-gensyms (next-state)
    (let ((tags (loop for form in tagbody when (atom form) collect form)))
      `(macrolet ((next-state (,next-state)
                    `(progn
                       (setf (ds-current-state ,',state) ',,next-state)
                       (go ,,next-state))))
         (tagbody
            ;; possibly could do better than a linear search here, but
            ;; if state machine is being interrupted often enough to
            ;; matter, it probably won't matter anyway :/ at most,
            ;; maybe define more commonly interrupted states earlier
            (ecase (ds-current-state ,state)
              ,@(loop for i in tags
                      collect `(,i (go ,i))))
            ,@tagbody
            #++
            ,@(loop for f in tagbody
                    collect f
                    when (atom f)
                      ;;collect `(format *debug-io* "=>~s~%" ',f) and
                      collect `(setf (ds-current-state ,state) ',f)))))))


(defparameter *stats* (make-hash-table))
(defun decompress (read-context state &key into)
  (declare (type (or null octet-vector) into)
           (optimize speed))
  (check-type into octet-vector)
  (assert into)
  (with-reader-contexts (read-context)
    (with-cached-state (state deflate-state save-state
                         partial-bits partial-bit-offset
                         tree-bits tree-offset
                         extra-bits-needed extra-bits-type
                         current-huffman-tree
                         )
      (symbol-macrolet ((ht-nodes (ht-nodes current-huffman-tree)))
        (with-bit-readers (state)
          (let ((count 0)
                ;;(ht-nodes (ht-nodes current-huffman-tree))
                (ht-scratch (make-huffman-tree)))
            (declare (type (simple-array (unsigned-byte 8) 1) into)
                     (fixnum count)
                     (type (simple-array ht-node-type 1) ht-nodes))
            (labels ((out-byte (x)
                       (setf (aref into count) x)
                       (setf count (wrap-fixnum (1+ count))))
                     (copy-history (n)
                       (declare (type fixnum n))
                       (let* ((o last-decoded-len/dist)
                              (d count)
                              (s (- d o))
                              (e (length into)))
                         (declare (type (and fixnum unsigned-byte) d e)
                                  (type fixnum s))
                         (cond
                           ((< s 0)
                            (error "no window?"))
                           ;; if copy won't fit (or oversized copy below
                           ;; might overrun buffer), use slow path for
                           ;; now
                           ((> (+ d n 8)
                               e)
                            (loop while (< d e)
                                  do (setf (aref into d) (aref into s))
                                     (setf d (1+ d))
                                     (setf s (1+ s)))
                            ;; todo: store state so it can continue
                            (when (< d (+ count n))
                              (error "output full")))
                           ;; to speed things up, we allow writing past
                           ;; current output index (but not past end of
                           ;; buffer), and read/write as many bytes at a
                           ;; time as possible.
                           ((> o 8)
                            (loop repeat (ceiling n 8)
                                  do (setf (nibbles:ub64ref/le into d)
                                           (nibbles:ub64ref/le into s))
                                     (setf d (wrap-fixnum (+ d 8)))
                                     (setf s (wrap-fixnum (+ s 8)))))
                           ((= o 8)
                            (loop with x = (nibbles:ub64ref/le into s)
                                  repeat (ceiling n 8)
                                  do (setf (nibbles:ub64ref/le into d)
                                           x)
                                     (setf d (wrap-fixnum (+ d 8)))))
                           ((> o 4)
                            (loop repeat (ceiling n 4)
                                  do (setf (nibbles:ub32ref/le into d)
                                           (nibbles:ub32ref/le into s))
                                     (setf d (wrap-fixnum (+ d 4)))
                                     (setf s (wrap-fixnum (+ s 4)))))

                           ((= o 1)
                            ;; if offset is 1, we are just repeating a
                            ;; single byte...
                            (loop with x of-type octet = (aref into s)
                                  repeat n
                                  do (setf (aref into d) x)
                                     (setf d (wrap-fixnum (1+ d)))))
                           ((= o 4)
                            (loop with x = (nibbles:ub32ref/le into s)
                                  with xx = (dpb x (byte 32 32) x)
                                  repeat (ceiling n 8)
                                  do (setf (nibbles:ub64ref/le into d) xx)
                                     (setf d (wrap-fixnum (+ d 8)))))
                           ((= o 3)
                            (loop repeat (ceiling n 2)
                                  do (setf (nibbles:ub16ref/le into d)
                                           (nibbles:ub16ref/le into s))
                                     (setf d (wrap-fixnum (+ d 2)))
                                     (setf s (wrap-fixnum (+ s 2)))))
                           ((= o 2)
                            (loop with x = (nibbles:ub16ref/le into s)
                                  with xx = (dpb x (byte 16 16) x)
                                  with xxxx = (dpb xx (byte 32 32) xx)
                                  repeat (ceiling n 8)
                                  do (setf (nibbles:ub64ref/le into d) xxxx)
                                     (setf d (wrap-fixnum (+ d 8))))))
                         ;; D may be a bit past actual value, so calculate
                         ;; correct offset
                         (setf count (wrap-fixnum (+ count n)))))
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
              (declare (inline out-byte store-dht)
                       (inline copy-history))


              (state-machine (state)

                :start-of-block
                ;; fixme: combine reads or split states
                (let ((final (the bit (bits 1 (error "foo!"))))
                      (type (the (unsigned-byte 2) (bits 2 (error "foo2")))))
                  (setf last-block-flag (plusp final))
                  (ecase type
                    (0 (next-state :uncompressed-block))
                    (1 ;; static huffman tree
                     (setf current-huffman-tree +static-huffman-tree+)
                                        ;(setf ht-nodes (ht-nodes current-huffman-tree))
                     (setf tree-bits (ht-len-start-bits current-huffman-tree))
                     (setf tree-offset 0)
                     (next-state :decode-huffman-entry))
                    (2 (next-state :dynamic-huffman-block))))

                :uncompressed-block
                (byte-align)
                ;; fixme: split this into multiple states
                (let ((s (bits 16 (error "foo3")))
                      (n (the (unsigned-byte 16) (bits 16 (error "foo4")))))
                  (assert (= n (ldb (byte 16 0) (lognot s))))
                  (loop repeat s
                        do (out-byte (the octet (bits 8 (error "5"))))))
                (next-state :block-end)

                :dynamic-huffman-block
                (setf dht-hlit (+ 257 (bits 5 (error "dhb1"))))
                (next-state :dynamic-huffman-block2)

                :dynamic-huffman-block2
                (let ((hdist (+ 1 (bits 5 (error "dhb2")))))
                  (setf dht-hlit+hdist (+ dht-hlit hdist)))
                (setf dht-lit/len/dist-index 0)
                (next-state :dynamic-huffman-block3)

                :dynamic-huffman-block3
                (setf dht-hclen (+ 4 (bits 4 (error "dhb3"))))
                (fill dht-len-codes 0)
                (setf dht-len-code-index 0)
                (next-state :dynamic-huffman-block-len-codes)

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
                (setf current-huffman-tree dht-len-tree)
                ;;(setf ht-nodes (ht-nodes current-huffman-tree))
                (setf extra-bits-type 2)
                (setf tree-bits (ht-len-start-bits current-huffman-tree))
                (setf tree-offset 0)
                (next-state :decode-huffman-entry)

                :block-end
                (if last-block-flag
                    (next-state :done)
                    (next-state :start-of-block))


                :decode-huffman-entry
                (let* ((bits (bits tree-bits (error "6")))
                       (node (aref ht-nodes (+ bits tree-offset))))
                  ;; test file shows ~ 1.5:1.3:0.5 for link:len/dist:literal
                  (ecase (ht-node-type node)
                    (#.+ht-link/end+
                     (if (ht-endp node)
                         (next-state :block-end)
                         (setf tree-bits (ht-link-bits node)
                               tree-offset (ht-link-offset node)))
                     (go :decode-huffman-entry))
                    (#.+ht-len/dist+
                     (let ((v (ht-value node)))
                       (cond
                         ((/= extra-bits-type 2)
                          ;; reading length or distance, with
                          ;; possible extra bits
                          (setf last-decoded-len/dist
                                (aref +len/dist-bases+ v))
                          (setf extra-bits-needed
                                (aref +extra-bits+ v))
                          (when (zerop extra-bits-needed)
                            (next-state :extra-bits2)))
                         ;; reading dynamic table lengths
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
                       (next-state :extra-bits1)))
                    (#.+ht-literal+
                     (out-byte (ht-value node))
                     (setf tree-bits (ht-len-start-bits current-huffman-tree))
                     (setf tree-offset 0)
                     (go :decode-huffman-entry))))

                :more-dht?
                (if (plusp dht-hlit+hdist)
                    (progn
                      (setf tree-bits (ht-len-start-bits current-huffman-tree))
                      (setf tree-offset 0))
                    (progn
                      (setf extra-bits-type 0)
                      (build-tree* (ds-dynamic-huffman-tree state)
                                   dht-lit/len/dist
                                   dht-hlit
                                   dht-lit/len/dist-index
                                   ht-scratch)
                      (setf extra-bits-type 0)
                      (setf current-huffman-tree (ds-dynamic-huffman-tree state))
                                        ;(setf ht-nodes (ht-nodes current-huffman-tree))
                      (setf tree-bits (ht-len-start-bits current-huffman-tree))
                      (setf tree-offset 0)))
                (next-state :decode-huffman-entry)

                :extra-bits1
                (let ((bits (bits extra-bits-needed (error "7"))))
                  (declare (type (unsigned-byte 16) bits))
                  (incf last-decoded-len/dist bits))
                (next-state :extra-bits2)

                :extra-bits2
                (ecase extra-bits-type
                  (0 ;; len
                   (setf bytes-to-copy last-decoded-len/dist)
                   ;; read dist
                   (setf tree-bits (ht-dist-start-bits current-huffman-tree))
                   (setf tree-offset (ht-dist-offset current-huffman-tree))
                   (setf extra-bits-type 1)
                   (next-state :decode-huffman-entry))
                  (1 ;; dist
                   (setf extra-bits-type 0)
                   (copy-history bytes-to-copy)
                   (setf tree-bits (ht-len-start-bits current-huffman-tree))
                   (setf tree-offset 0)
                   (next-state :decode-huffman-entry))
                  (2 ;; dht
                   (repeat-dht last-decoded-len/dist)
                   (next-state :more-dht?)))


                :done)
              into)))))))
