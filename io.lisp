(in-package #:3bz)

(deftype octet () '(unsigned-byte 8))
(deftype octet-vector () '(simple-array octet (*)))
;; we restrict size of these types a bit more on 64 bit platforms to
;; ensure intermediate results stay in reasonable range for
;; performance. 32bit probably needs tuned, might want to allow larger
;; than fixnum offsets for FFI use with implementations with small
;; fixnums?
(deftype size-t () (if (= 8 (cffi:foreign-type-size :pointer))
                       `(unsigned-byte
                         ,(min 60 (1- (integer-length most-positive-fixnum))))
                       `(unsigned-byte
                         ,(min 30 (integer-length most-positive-fixnum)))))
;; slightly larger so incrementing a size-t still fits
(deftype offset-t () (if (= 8 (cffi:foreign-type-size :pointer))
                         `(unsigned-byte
                           ,(min 61 (integer-length most-positive-fixnum)))
                         `(unsigned-byte
                           ,(min 31 (integer-length most-positive-fixnum)))))

;; typed container for offsets and bounds of current input source, and
;; remaining bits of partially read octets
(defstruct (context-boxes (:conc-name cb-))
  ;; start of 'active' region of buffer
  (start 0 :type size-t)
  ;; end of 'active' region of buffer
  (end 0 :type size-t)
  ;; offset of next unread byte, (<= start offset end)
  (offset 0 :type size-t))


(defmacro context-common ((boxes) &body body)
  `(macrolet ((pos ()
                `(cb-offset ,',boxes))
              (end ()
                `(cb-end ,',boxes))
              (%octet (read-form
                       &optional (eob-form
                                  '(error "read past end of buffer")))
                `(progn
                   (when (>= (pos) (end))
                     ,eob-form)
                   (prog1
                       ,read-form
                     (incf (pos)))))
              (octets-left ()
                `(- (cb-end ,',boxes) (pos))))
     ,@body))


(defclass octet-vector-context ()
  ((octet-vector :reader octet-vector :initarg :octet-vector)
   (boxes :reader boxes :initarg :boxes)))

(defclass octet-stream-context ()
  ((octet-stream :reader octet-stream :initarg :octet-stream)
   (boxes :reader boxes :initarg :boxes)))

(defun valid-octet-stream (os)
  (and (typep os 'stream)
       (subtypep (stream-element-type os) 'octet)
       (open-stream-p os)
       (input-stream-p os)))

(defclass octet-pointer ()
  ((base :reader base :initarg :base)
   (size :reader size :initarg :size) ;; end?
   (scope :reader scope :initarg :scope)))

(defmacro with-octet-pointer ((var pointer size) &body body)
  (with-gensyms (scope)
    (once-only (pointer size)
     `(let* ((,scope (cons t ',var)))
        (unwind-protect
             (let ((,var (make-instance 'octet-pointer :base ,pointer
                                                       :size ,size
                                                       :scope ,scope)))
               ,@body)
          (setf (car ,scope) nil))))))

(defun valid-octet-pointer (op)
  (and (car (scope op))
       (not (cffi:null-pointer-p (base op)))
       (plusp (size op))))

(defclass octet-pointer-context ()
  ((op :reader op :initarg :op)
   (pointer :reader %pointer :initarg :pointer)
   (boxes :reader boxes :initarg :boxes)))


(defmacro with-vector-context ((context) &body body)
  (with-gensyms (boxes vector)
    (once-only (context)
      `(let* ((,boxes (boxes ,context))
              (,vector (octet-vector ,context)))
         (declare (optimize speed)
                  (ignorable ,vector ,boxes)
                  (type context-boxes ,boxes))
         (check-type ,vector octet-vector)
         (locally (declare (type octet-vector ,vector))
           (context-common (,boxes)
             (macrolet ((octet (&optional (eob-form
                                           '(error "read past end of buffer")))
                          `(%octet (aref ,',vector (pos))
                                   ,eob-form))
                        ;; read up to 8 octets in LE order, return
                        ;; result + # of octets read as multiple
                        ;; values
                        (word64 ()
                          (with-gensyms (available result)
                            `(let ((,available (octets-left)))
                               (if (>= ,available 8)
                                   (let ((,result (nibbles:ub64ref/le
                                                   ,',vector (pos))))
                                     (incf (pos) 8)
                                     (values ,result 8))
                                   (let ((,result 0))
                                     (loop
                                       for i fixnum below ,available
                                       do (setf ,result
                                                (ldb (byte 64 0)
                                                     (logior
                                                      ,result
                                                      (ash
                                                       (aref ,',vector
                                                             (+ (pos) i))
                                                       (* i 8))))))
                                     (incf (pos) ,available)
                                     (values ,result ,available)))))))
               ,@body)))))))

(defmacro with-stream-context ((context) &body body)
  (with-gensyms (boxes stream)
    (once-only (context)
      `(let* ((,boxes (boxes ,context))
              (,stream (octet-stream ,context)))
         (declare (optimize speed)
                  (ignorable ,stream ,boxes)
                  (type context-boxes ,boxes))
         (assert (valid-octet-stream ,stream))
         (context-common (,boxes)
           (macrolet (;; override POS/SET-POS for streams
                      (pos ()
                        `(file-position ,',stream))
                      (octet (&optional (eob-form
                                         '(error "read past end of buffer")))
                        (with-gensyms (p)
                          `(progn
                             ;; go through some extra work to avoid
                             ;; optimization notes..
                             (let ((,p (pos)))
                               (when ,p
                                 (locally (declare (type size-t ,p))
                                   (when (>= ,p (end))
                                     ,eob-form))))
                             (read-byte ,',stream))))
                      (word64 ()
                        (with-gensyms (available result)
                          `(locally (declare (optimize (speed 1)))
                             (let ((,available (- (end) (pos))))
                               (if (>= ,available 8)
                                   (values (nibbles:read-ub64/le ,',stream) 8)
                                   (let ((,result 0))
                                     (declare (type (unsigned-byte 64) ,result)
                                              (type (mod 8) ,available))
                                     (loop
                                       for i fixnum below (min 8 ,available)
                                       do (setf (ldb (byte 8 (* i 8))
                                                     ,result)
                                                (octet)))
                                     (values ,result ,available))))))))
             ,@body))))))

(defmacro with-ffi-context ((context) &body body)
  (with-gensyms (boxes pointer)
    (once-only (context)
      `(let* ((,boxes (boxes ,context))
              (,pointer (base (op ,context))))
         (declare (optimize speed)
                  (ignorable ,pointer ,boxes)
                  (type context-boxes ,boxes))
         (assert (valid-octet-pointer (op ,context)))
         (context-common (,boxes)
           (macrolet ((octet (&optional (eob-form
                                         '(error "read past end of buffer")))
                        `(%octet (cffi:mem-ref ,',pointer :uint8 (pos))
                                 ,eob-form))
                      (word64 ()
                        (with-gensyms (available result)
                          `(let ((,available (octets-left)))
                             (if (>= ,available 8)
                                 (let ((,result (cffi:mem-ref
                                                 ,',pointer :uint64 (pos))))
                                   (incf (pos) 8)
                                   (values ,result 8))
                                 (let ((,result 0))
                                   (declare (type (unsigned-byte 64) ,result))
                                   (loop
                                     for i fixnum below (min 8 ,available)
                                     do (setf ,result
                                              (ldb (byte 64 0)
                                                   (logior
                                                    ,result
                                                    (ash
                                                     (cffi:mem-ref
                                                      ,',pointer
                                                      :uint8
                                                      (+ (pos) i))
                                                     (* i 8))))))
                                   (incf (pos) ,available)
                                   (values ,result ,available)))))))
             ,@body))))))

(defmacro with-reader-contexts ((in) &body body)
  `(let ((@context ,in))
     (declare (ignorable @context))
     (etypecase ,in
       (octet-vector-context
        (with-vector-context (,in)
          ,@body))
       (octet-stream-context
        (with-stream-context (,in)
          ,@body))
       (octet-pointer-context
        (with-ffi-context (,in)
          ,@body)))))
