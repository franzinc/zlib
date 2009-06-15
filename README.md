zlib: Compression Library for Common Lisp
=========================================

Table of contents
-----------------

 * Description
 * Author
 * Author comments
 * Documentation
 * Platforms
 * Dependencies
 * Installation
 * Configuration
 * Licence
 * Notes
 * Examples
 * Franz Inc. Open Source Info

Description
-----------

Deflation is a means of compressing an octet sequence that combines
the LZ77 algorithm for marking common substrings and Huffman coding to
take advantage of the different frequencies of occurence of byte
sequences in the file. This algorithm may not be as easy to understand
or as efficient as the LZW compression algorithm but Deflate does have
the important advantage in that it is not patented. Thus Deflate
widely used. Presently it's the most common compression method used by
Windows Zip programs (e.g. Winzip) and in the Unix gzip program. Java
jar files, being just zip files, also use this compression method.


Author
------

John Foderaro, Franz Inc.

Author comments
---------------

If you have any doubt that Lisp is a better language for writing bit
level algorithms you need only compare the source for this version of
the Inflate algorithm with the C version found in programs such as
gzip. The C code is essentially unreadable.

Platforms
----------

All Common Lisps, but tested on Allegro Common Lisp.

Dependencies
------------

None for Allegro Common Lisp.  For non-Allegro Common Lisps the
acl-compat package is required, which provides the
[if*](http://www.franz.com/~jkf/ifstar.txt) macro.

Installation
------------

Load the inflate source file:

    (load "/path/to/your/inflate.cl")

Configuration
-------------

None.

Documentation
-------------

### Objects:

#### inflate-stream

open a stream p to a file containing the compressed data.  If this
file has a gzip header then: 

    (skip-gzip-header p)
    (make-instance 'inflate-stream :input-handle p)

will return a stream which can be read to recover the uncompressed data.

### Functions:

#### inflate (input-stream output-stream)

inflates the input-stream into the output-stream.  Both streams should
be of type unsigned-byte 8.  Closing the inflate-stream will not close
stream p.  That must be done separately.

#### skip-gzip-header (input-stream)

If the input stream is positioned on the header of a gzip'ed file then
the header is skipped.  If the input-stream is not positioned on the
gzip header then nothing is done.

License
-------

The zlib source code is licensed under the terms of the 
[Lisp Lesser GNU Public License](http://opensource.franz.com/preamble.html), 
known as the LLGPL. The LLGPL consists of a preamble and the LGPL. Where these 
conflict, the preamble takes precedence.  This project is referenced in the 
preamble as the LIBRARY.

Notes
-----

At present this module only contains the code to uncompress a deflated
byte sequence. We may write code to do deflation in the future but at
present have no pressing need for this functionality.

We will be adding a simple-streams interface soon. With this interface
you can layer the decompressor onto a simple-stream and decompress
while you read bytes or characters from the stream.


Examples and Information
------------------------

See the forms at the end of the inflate.cl file that is part of this
project for examples and tests.

Franz Open Source Info
----------------------

This project's homepage is <http://opensource.franz.com>. There is an 
informal community support and development mailing list 
[opensource@franz.com](http://opensource.franz.com/mailinglist.html) 
for these open source projects. We encourage you to take advantage by 
subscribing to the list.  Once you're subscribed, email to 
<opensource@franz.com> with your questions, comments, suggestions, 
and patches.
