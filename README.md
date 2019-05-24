Yet another CL impementation of [rfc1951
deflate](https://tools.ietf.org/html/rfc1951) decompression
(optionally with [rfc1950 deflate](https://tools.ietf.org/html/rfc1950)
or [rfc1952
gzip](https://tools.ietf.org/html/rfc1952https://tools.ietf.org/html/rfc1952)
wrappers), with support for reading from foreign pointers (for use with
mmap and similar, etc), and from CL octet vectors and streams.

### Still somewhat WIP, but approaching usability.

Performance is somewhere between FFI to libz and chipz, still needs
some low-level optimization of copy routines and checksums.

API isn't completely stable yet, needs some actual use to figure out
the details.