(defpackage 3bz
  (:use :cl)
  (:import-from :alexandria
                #:with-gensyms
                #:once-only
                #:ensure-list)
  (:export
   #:decompress
   #:decompress-vector
   #:with-octet-pointer
   #:make-octet-vector-context
   #:make-octet-stream-context
   #:make-octet-pointer-context
   #:make-deflate-state
   #:make-zlib-state
   #:make-gzip-state
   #:finished
   #:input-underrun
   #:output-overflow
   #:%resync-file-stream
   #:replace-output-buffer))

