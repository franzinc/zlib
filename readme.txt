Inflate - file compression algorithm
	 
Description

Deflation is a means of compressing an octet sequence that combines
the LZ77 algorithm for marking common substrings and Huffman coding to
take advantage of the different frequencies of occurence of byte
sequences in the file. This algorithm may not be as easy to understand
or as efficient as the LZW compression algorithm but Deflate does have
the important advantage in that it is not patented. Thus Deflate
widely used. Presently it's the most common compression method used by
Windows Zip programs (e.g. Winzip) and in the Unix gzip program. Java
jar files, being just zip files, also use this compression method.

At present this module only contains the code to uncompress a deflated
byte sequence. We may write code to do deflation in the future but at
present have no pressing need for this functionality.

We will be adding a simple-streams interface soon. With this interface
you can layer the decompressor onto a simple-stream and decompress
while you read bytes or characters from the stream.

Author: John Foderaro, Franz Inc.

If you have any doubt that Lisp is a better language for writing bit
level algorithms you need only compare the source for this version of
the Inflate algorithm with the C version found in programs such as
gzip. The C code is essentially unreadable.

Release History

release 1.0 - November 12, 2001 - initial release as open source.

Documentation:
The documentation is at the beginning of the source file.

References

    * The Deflate Specification v1.3: http://www.gzip.org/zlib/rfc-deflate.html

Platform

We've tested this code on Allegro Common Lisp versions 6.0 and 6.1. It
will likely work on earlier versions of ACL and other Common Lisps.

Building

The source file is just compiled and loaded into Lisp. The functions
defined are in then found in the util.zip package.

Dependencies: None.

Links

    * The source for the popular if* macro used in this code to
      increase readabilty: http://www.franz.com/~jkf/ifstar.txt

License

The deflate source code is licensed under the terms of the Lisp Lesser
GNU Public License, known as the LLGPL. The LLGPL consists of a
preamble and the LGPL. Where these conflict, the preamble takes
precedence. deflate is referenced in the preamble as the LIBRARY.

Open Source

This project is hosted on the http://opensource.franz.com site. There
is an informal community support and development mailing list
(opensource@franz.com) for these open source projects. We encourage
you to take advantage by subscribing to the list. Click here to
subscribe or unsubscribe. Once you're subscribed, send email to
opensource@franz.com with your questions, comments, suggestions, and
patches.
