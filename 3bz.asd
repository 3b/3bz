(defsystem :3bz
  :description "deflate decompressor"
  :depends-on (cffi alexandria mmap nibbles)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components
  ((:file "package")
   (:file "util")
   (:file "constants")
   (:file "types")
   (:file "huffman-tree")
   (:file "io")
   (:file "deflate")
   (:file "checksums")
   (:file "zlib")
   (:file "gzip")
   (:file "api")))

