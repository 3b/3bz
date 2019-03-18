Yet another CL impementation of [rfc1951
deflate](https://tools.ietf.org/html/rfc1951) decompression, with
support for reading from foreign pointers (for use with mmap and
similar, etc), and from CL octet vectors and streams.

## Currently work in progress, you probably don't want to use this unless you want to help finish it.

seems to read more bitstreams than chipz (possibly slightly
faster as well), but still needs lots of work on performance.

API and error handling are currently very limited as well.

If performance improves enough to be useful, will probably add support
for zlib/gzip headers and checksums, as well as more flexible
APIs. Possibly also some other compression formats.