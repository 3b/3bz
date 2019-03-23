(in-package 3bz)


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

(defparameter *fixed-lit/length-table*
  (concatenate 'code-table-type
               (make-array (1+ (- 143 0)) :initial-element 8)
               (make-array (1+ (- 255 144)) :initial-element 9)
               (make-array (1+ (- 279 256)) :initial-element 7)
               (make-array (1+ (- 287 280)) :initial-element 8)))

(defparameter *fixed-dist-table*
  (coerce (make-array 32 :initial-element 5) 'code-table-type))

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
#++(dump-tree +static-huffman-tree+)
