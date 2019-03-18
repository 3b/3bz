(in-package 3bz)

#++(ql:quickload '(salza2 flexi-streams chipz deoxybyte-gzip))


(defvar *foo* nil)
(defvar *chipz* nil)
(defvar *3bz* nil)
#++
(prog1 nil
  (push *foo* *chipz*))
#++
(prog1 nil
  (push *foo* *3bz*))

(let* ((d (time
           (alexandria:read-file-into-byte-vector "/tmp/tmp"))
          #++(setf *foo*
                   (time
                    (map-into (make-array (expt 2 24) :element-type 'octet)
                              (lambda () (random 225))))))
       (tmp (make-array (length d) :element-type 'octet
                                   :initial-element 0))
       (v #++(time
              (salza2:compress-data d 'salza2:deflate-compressor))
          (time
           (multiple-value-bind (x r w)
               (gz:deflate-vector d tmp :compression 9 :suppress-header t)
             (subseq x 0 w))))
       (c (make-instance 'octet-vector-context
                         :octet-vector v
                         :boxes (make-context-boxes :end (length v))))
       (state (make-deflate-state)))
  #++(time (dump-deflate v "-sirqq"))
  (format t "chipz:~%")
  (with-simple-restart (continue "continue")
    (let ((x (time (chipz:decompress nil 'chipz:deflate v))))
      (assert (equalp d x))))
  (fill tmp 0)
  (format t "3bz:~%")
  (let ((x (time (decompress c state :into tmp))))
    (assert (equalp d (if (consp x)
                          (time (apply 'concatenate 'octet-vector x))
                          x))))
  (fill tmp 0)
  (format t "gz:~%")
  (let ((x (time (gz:inflate-vector v tmp :suppress-header t))))
    (assert (equalp x d)))
  (print (length v))
  nil)
