(in-package 3bz)

(deftype ht-bit-count-type ()'(unsigned-byte 4))
(deftype ht-offset-type ()'(unsigned-byte 11))
(deftype ht-node-type ()'(unsigned-byte 16))
(deftype ht-node-array-type () `(simple-array ht-node-type (,+max-tree-size+)))

(deftype code-table-type () '(simple-array (unsigned-byte 4) 1))
