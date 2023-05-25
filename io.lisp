(in-package #:3bz)

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
             (macrolet (;; read up to 8 octets in LE order, return
                        ;; result + # of octets read as multiple
                        ;; values
                        (word64 ()
                          (with-gensyms (available result)
                            `(let ((,available (octets-left)))
                               (if (>= ,available 8)
                                   (let ((,result (ub64ref/le
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
                                     (values ,result ,available))))))
                        (word32 ()
                          (with-gensyms (available result)
                            `(let ((,available (octets-left)))
                               (if (>= ,available 4)
                                   (let ((,result (ub32ref/le
                                                   ,',vector (pos))))
                                     (incf (pos) 4)
                                     (values ,result 4))
                                   (let ((,result 0))
                                     (loop
                                       for i of-type (unsigned-byte 2) below (min 4 ,available)
                                       do (setf ,result
                                                (ldb (byte 32 0)
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
                                                (read-byte ,',stream)))
                                     (values ,result ,available)))))))
                      (word32 ()
                        (with-gensyms (available result)
                          `(locally (declare (optimize (speed 1)))
                             (let ((,available (- (end) (pos))))
                               (if (>= ,available 4)
                                   (values (nibbles:read-ub32/le ,',stream) 4)
                                   (let ((,result 0))
                                     (declare (type (unsigned-byte 64) ,result)
                                              (type (mod 4) ,available))
                                     (loop
                                       for i fixnum below (min 4 ,available)
                                       do (setf (ldb (byte 8 (* i 8))
                                                     ,result)
                                                (read-byte ,',stream)))
                                     (values ,result ,available))))))))
             ,@body))))))

(defmacro with-buffered-stream-context ((context) &body body)
  (with-gensyms (boxes stream vector vector-pos vector-end buffer-size)
    (once-only (context)
      `(let* ((,boxes (boxes ,context))
              (,stream (octet-stream ,context))
              (,buffer-size (buffer-size ,context))
              (,vector (make-array ,buffer-size
                                   :element-type '(unsigned-byte 8)))
              (,vector-end 0)
              (,vector-pos 0))
         (declare (optimize speed)
                  (ignorable ,stream ,boxes)
                  (type context-boxes ,boxes)
                  (type size-t ,buffer-size ,vector-pos ,vector-end))
         (assert (valid-octet-stream ,stream))
         (context-common (,boxes)
           (macrolet ((ensure-valid ()
                        `(when (and (= ,',vector-end ,',vector-pos) (plusp (octets-left)))
                           (setf ,',vector-end (read-sequence ,',vector ,',stream
                                                              :end (min ,',buffer-size (- (end) (pos))))
                                 ,',vector-pos 0)))
                      (word64 ()
                        (with-gensyms (available result locally-available)
                          `(progn
                             (ensure-valid)
                             (let ((,available (octets-left))
                                   (,locally-available (- ,',vector-end ,',vector-pos)))
                               (declare (type offset-t ,locally-available))
                               (if (and (>= ,available 8)
                                        (>= ,locally-available 8))
                                   (let ((,result (ub64ref/le
                                                   ,',vector ,',vector-pos)))
                                     (incf (pos) 8)
                                     (incf ,',vector-pos 8)
                                     (values ,result 8))
                                   (let ((,result 0))
                                     (loop
                                       for i fixnum below (min 8 ,available)
                                       do (ensure-valid)
                                          (setf ,result
                                                (ldb (byte 64 0)
                                                     (logior
                                                      ,result
                                                      (ash
                                                       (aref ,',vector
                                                             (+ ,',vector-pos i))
                                                       (* i 8)))))
                                          (incf ,',vector-pos))
                                     (incf (pos) ,available)
                                     (values ,result ,available)))))))
                      (word32 ()
                        (with-gensyms (available result)
                          `(let ((,available (octets-left)))
                             (ensure-valid)
                             (if (>= ,available 4)
                                 (let ((,result (ub32ref/le
                                                 ,',vector ,',vector-pos)))
                                   (incf (pos) 4)
                                   (incf ,',vector-pos 4)
                                   (values ,result 4))
                                 (let ((,result 0))
                                   (loop
                                     for i of-type (unsigned-byte 2) below (min 4 ,available)
                                     do (setf ,result
                                              (ldb (byte 32 0)
                                                   (logior
                                                    ,result
                                                    (ash
                                                     (aref ,',vector
                                                           (+ ,',vector-pos i))
                                                     (* i 8))))))
                                   (incf (pos) ,available)
                                   (incf ,',vector-pos ,available)
                                   (values ,result ,available)))))))
             ,@body))))))



(defmacro defun-with-reader-contexts (base-name lambda-list (in) &body body)
  `(progn
     ,@(with-standard-io-syntax
         (loop for cc in '(vector buffered-stream stream pointer)
               for w = (find-symbol (format nil "~a-~a-~a" 'with cc 'context)
                                    (find-package :3bz))
               for n = (intern (format nil "~a/~a" base-name cc)
                               (find-package :3bz))
               collect `(defun ,n ,lambda-list
                          (,w (,in)
                              (let ()
                                ,@body)))))
     (defun ,base-name ,lambda-list
       (etypecase ,in
         ,@(with-standard-io-syntax
             (loop for cc in '(vector buffered-stream stream pointer)
                   for ct = (find-symbol (format nil "~a-~a-~a" 'octet cc 'context)
                                         (find-package :3bz))
                   for n = (find-symbol (format nil "~a/~a" base-name cc)
                                        (find-package :3bz))
                   collect `(,ct (,n ,@lambda-list))))))))

(defmacro with-reader-contexts ((context) &body body)
  `(etypecase ,context
     (octet-vector-context
      (with-vector-context (,context)
        ,@body))
     (octet-pointer-context
      (with-pointer-context (,context)
        ,@body))
     (octet-buffered-stream-context
      (with-buffered-stream-context (,context)
        ,@body))
     (octet-stream-context
      (with-stream-context (,context)
        ,@body))))

