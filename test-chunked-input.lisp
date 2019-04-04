(in-package 3bz)


#++
(ql:quickload '(deoxybyte-gzip))

#++
(let ((*default-pathname-defaults* (asdf:system-relative-pathname '3bz "")))
  (with-open-file (o "test.deflated" :element-type 'octet :direction :output
                                     :if-does-not-exist :create :if-exists :error)
    (let* ((i (alexandria:read-file-into-byte-vector "deflate.lisp"))
           (tmp (make-array (length i) :element-type 'octet
                                       :initial-element 0)))
      (multiple-value-bind (x r w)
          (gz:deflate-vector i
            tmp :compression 9
            :suppress-header t)
        (declare (ignore r))
        (nibbles:write-ub64/le (length i) o)
        (write-sequence (subseq x 0 w) o)))))

(defparameter *test-file*
  (let ((f (alexandria:read-file-into-byte-vector (asdf:system-relative-pathname '3bz "test.deflated"))))
    (list (nibbles:ub64ref/le f 0)
          (subseq f 8))))

(defun test-chunked (decompressed-size vector generator)
  (let ((l (length vector))
        (o 0)
        (state (make-deflate-state))
        (tmp (make-array decompressed-size :element-type 'octet
                                           :initial-element 0)))
    (loop for end = (min l (+ o (funcall generator)))
          for s = (unless (= o l)
                    (subseq vector o end))
          for c = (make-instance 'octet-vector-context
                                 :octet-vector s
                                 :boxes (make-context-boxes :end (length s)))
          while s
          do (decompress c state :into tmp)
             (setf o end))
    tmp))

(defparameter *foo* nil)
(defparameter *c* 0)
(loop
  repeat 3000
  while
  (let ((ref (gz:inflate-vector (second *test-file*)
                                (make-array (first *test-file*)
                                            :element-type 'octet)
                                :suppress-header t)))
    (setf *foo* nil)
    (incf *c*)
    (equalp
    ref
    (test-chunked (first *test-file*) (second *test-file*)
                  (lambda ()
                    (let ((r (random 12345)))
                      (push r *foo*)
                      r)))))
  count t)

