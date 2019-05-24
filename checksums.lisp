(in-package 3bz)

(defun adler32 (buf end s1 s2)
  (declare (type octet-vector buf)
           (type (unsigned-byte 16) s1 s2)
           (optimize speed))
  ;; with 32bit accumulators, we need to do the MOD every 5552 adds.
  ;; with 64bit, every 380368439.  formula = (+ (* (1+ n) 65520) (* (/
  ;; (* n (1+ n)) 2) 255))
  (let* ((unroll 16) ;; adjust UNROLL call below if changing this
         (chunk-size ;(* unroll (floor 5552 unroll))
           (* unroll (floor 380368439 unroll)))
         (s1 s1)
         (s2 s2))
    (declare (type (unsigned-byte 64) s1 s2))

    (assert (<= end (length buf)))
    (unless (zerop end)
      (loop
        with start fixnum = 0
        for c fixnum = (max 0
                            (min chunk-size
                                 (- end start)))
        do (macrolet ((a (i)
                        `(progn
                           (setf s1 (ldb (byte 64 0)
                                         (+ s1
                                            (locally
                                                (declare (optimize (safety 0)))
                                              (aref buf ,i)))))
                           (setf s2 (ldb (byte 64 0) (+ s2 s1)))))
                      (unroll (n)
                        `(progn
                           ,@(loop for x below n
                                   collect `(a (+ i ,x))))))
             (if (and (zerop (mod c unroll))
                      (plusp c))
                 (loop for i of-type fixnum from start
                         below (min (+ start c)
                                    (* unroll (floor (length buf) unroll)))
                       by unroll
                       do (unroll 16)) ;; adjust variable above if changing
                 (loop for i fixnum from start below (min (+ start c)
                                                          (length buf))
                       do (a i))))
           (incf start c)
           (setf s1 (mod s1 +adler32-prime+)
                 s2 (mod s2 +adler32-prime+))
        while (< start end)))
    (values s1 s2)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun generate-crc32-table ()
    (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
      (loop for n below (length table)
            do (setf (aref table n)
                     (let ((c n))
                       (loop for k below 8
                             if (oddp c)
                               do (setf c (logxor #xedb88320 (ash c -1)))
                             else
                               do (setf c (ash c -1)))
                       c)))
      table)))

(declaim (type (simple-array (unsigned-byte 32) (256)) +crc32/table+))
(alexandria:define-constant +crc32/table+
    (generate-crc32-table) :test 'equalp)


(defun crc32/table (buf end crc)
  (declare (type octet-vector buf)
           (type fixnum end)
           (type (unsigned-byte 32) crc)
           (optimize speed))
  (let ((crc (logxor crc #xffffffff)))
    (declare (type (unsigned-byte 32) crc))
    (loop for b across buf
          repeat end
          do (setf crc
                   (logxor (ldb (byte 24 8) crc)
                           (aref +crc32/table+
                                 (ldb (byte 8 0)
                                      (logxor crc b))))))
    (logxor crc #xffffffff)))
