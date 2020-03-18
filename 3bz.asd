(defsystem :3bz
  :description "deflate decompressor"
  :depends-on (alexandria
               (:feature (:not :mezzano) cffi)
               (:feature (:not :mezzano) mmap)
               trivial-features
               nibbles)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components
  ((:file "package")
   (:file "util")
   (:file "constants")
   (:file "types")
   (:file "huffman-tree")
   (:file "io-common")
   (:file "io-nommap" :if-feature :mezzano)
   (:file "io-mmap" :if-feature (:not :mezzano))
   (:file "io")
   (:file "deflate")
   (:file "checksums")
   (:file "zlib")
   (:file "gzip")
   (:file "api")))

