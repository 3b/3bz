(in-package 3bz)

;;; some tuning parameters, and functions used in #+#.(...) to control
;;; features used by code

;;; deflate code tries to read/write/copy 64bits at a time, and uses
;;; ub64 buffer for bits when doing non-octet-unaligned reads, but
;;; that's slow if ub64 is a bignum, so this is used to switch to ub32
;;; where possible
(defun use-ub64 ()
  '(:or)
  #+ (and (or x86-64 ppc64 sparc64 hppa64)
          (not (or ccl abcl)))
  '(:and))

;;; similarly, adler32 checksum accumulates as many bytes as possible
;;; before doing mod, so we can either use :ub64, :ub32 or :fixnum
;;; versions of adler32 code depending on which is fastest
(defun use-adler32 (version)
  (if (eql version
           ;; ub64 is fastest on 64bit sbcl
           #+ (and sbcl x86-64)
           :ub64
           ;; for now, just using fixnum elsewhere until there are
           ;; proper benchmarks. not sure if ub32 is faster than
           ;; fixnum anywhere, or if fixnum is good enough
           #- (or abcl (and sbcl x86-64))
           :fixnum
           #+ abcl
           :ub32
           )
      '(:and)
      '(:or)))
;;; adler32 checksum is unrolled a bit to reduce loop overhead, this
;;; specifies how many iterations to unroll
;; todo: set this for more combinations of cpu/implementation once
;; there are benchmarks
(defconstant +adler32-unroll+
  #+mezzano 5
  #+sbcl 32
  #- (or sbcl mezzano) 8)

