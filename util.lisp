(in-package 3bz)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cached-struct-defs* (make-hash-table)))

(defmacro defstruct-cached (name-and-options &body slots)
  `(progn
     (defstruct ,name-and-options
       ,@slots)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       ,(destructuring-bind (name &rest options)
            (alexandria:ensure-list name-and-options)
          (let ((conc-name (cadr (assoc :conc-name options))))
            (unless conc-name
              (setf conc-name (format nil "~a" name)))
            (flet ((accessor (slot)
                     (intern (format nil "~a~a" conc-name slot))))
              `(setf (gethash ',NAME *cached-struct-defs*)
                     ',(loop for (slot init . keys) in slots
                             for type = (getf keys :type)
                             collect (list slot (accessor slot) type)))))))))

(defmacro with-cached-state ((struct type save-state-fun) &body body)
  (let ((slots (gethash type *cached-struct-defs*)))
    (assert slots)
    `(let ,(loop for (var accessor) in slots
                 collect `(,var (,accessor ,struct)))
       (declare ,@(loop for (var nil type) in slots
                        when type collect `(type ,type ,var)
                        collect `(ignorable ,var)))
       (flet ((,save-state-fun ()
                ,@(loop for (var accessor) in slots
                        collect `(setf (,accessor ,struct) ,var))))
         (declare (ignorable #',save-state-fun))
         ,@body))))


(defmacro wrap-fixnum (x)
  ;; a few places we already checked something will be a fixnum (for
  ;; example an array index in a loop), so tell the compiler it doesn't
  ;; need to check for bignums
  `(ldb (byte #. (integer-length most-positive-fixnum) 0) ,x))

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
