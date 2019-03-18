(defsystem :3bz
  :description "deflate decompressor"
  :depends-on (cffi alexandria mmap)
  :serial t
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :components
  ((:file "package")
   (:file "io")
   (:file "deflate")))

