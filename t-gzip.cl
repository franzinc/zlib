;; See the file LICENSE for the full license governing this code.

(eval-when (compile load eval)
  (require :deflate)
  (require :inflate)
  (require :test))

(in-package :test)

(defun test-zlib ()
  (test-gzip)
  (test-bug25005)
  )

(defun deflate-file (input-filename output-filename &optional (type :gzip))
  (with-open-file (in input-filename :direction :input)
    (with-open-file (out output-filename 
		     :direction :output
		     :if-exists :supersede)
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
	    (deflate (make-instance 'util.zip::deflate-stream 
		       :target out
		       :compression type)))
	(loop
	  (let ((bytes (read-vector buffer in)))
	    
	    (if* (zerop bytes) then (return))
	    
	    (let ((pos 0))
	      
	      (loop
		(setq pos (write-vector buffer deflate :start pos :end bytes))
		(if* (>= pos bytes) then (return)))
	      )))
	
	; finish compression
	(close deflate)))))

(defun inflate-file (input-filename output-filename &optional (type :gzip))
  (with-open-file (in input-filename :direction :input)
    (with-open-file (out output-filename 
		     :direction :output
		     :if-exists :supersede)
      (format t ";; Inside inflate-file~%")
      (let ((inflate (make-instance 'util.zip::inflate-stream
		       :compression type
		       :input-handle in))
	    byte)
	(while (setq byte (read-byte inflate nil nil))
	  (write-byte byte out))))))

(defun deflate-test (input-filename)
  ;; compress input-file to temp-file1, uncompress it back to temp-file2
  ;; and compare temp-file2 to input-filename, error if not same.
  (flet ((cygwin-namestring (p)
	   #+mswindows (substitute #\/ #\\ (namestring p))
	   #-mswindows p))
    (let (temp-file1 temp-file2)
      (unwind-protect
	  (progn
	    (setq temp-file1 (sys:make-temp-file-name "deflate1x"))
	    (setq temp-file2 (sys:make-temp-file-name "deflate2x"))
	    (format t "; compress test on ~a~%"
		    (enough-namestring input-filename))
	    (deflate-file input-filename temp-file1)
	    (format t "; uncompress ~a to ~a~%" temp-file1 temp-file2)
	    (or (eql 0 (run-shell-command
			(format nil "sh -c 'gunzip -d < ~a > ~a'"
				(cygwin-namestring temp-file1)
				(cygwin-namestring temp-file2))
			:show-window :hide))
		(error "gunzip failed on ~a" temp-file1))
	    ;;(format t "; compare ~a to ~a~%" input-filename temp-file2)
	    (test-t (excl::compare-files input-filename temp-file2)))
	(when temp-file1 (ignore-errors (delete-file temp-file1)))
	(when temp-file2 (ignore-errors (delete-file temp-file2)))))))

(defun inflate-test (input-filename)
  ;; compress input-file to temp-file1, uncompress it back to temp-file2
  ;; and compare temp-file2 to input-filename, error if not same.
  (flet ((cygwin-namestring (p)
	   #+mswindows (substitute #\/ #\\ (namestring p))
	   #-mswindows p))
    (let (temp-file1 temp-file2)
      (unwind-protect
	  (progn
	    (setq temp-file1 (sys:make-temp-file-name "inflate1x"))
	    (setq temp-file2 (sys:make-temp-file-name "inflate2x"))
	    (format t "; uncompress test on ~a~%"
		    (enough-namestring input-filename))
	    (format t "; compress ~a to ~a~%" temp-file1 temp-file2)
	    (or (eql 0 (run-shell-command
			(format nil "sh -c 'gzip -c ~a > ~a'"
				(cygwin-namestring input-filename)
				(cygwin-namestring temp-file1))
			:show-window :hide))
		(error "gzip failed on ~a" temp-file1))
	    (inflate-file temp-file1 temp-file2)
	    ;;(format t "; compare ~a to ~a~%" input-filename temp-file2)
	    (test-t (excl::compare-files input-filename temp-file2)))
	(when temp-file1 (ignore-errors (delete-file temp-file1)))
	(when temp-file2 (ignore-errors (delete-file temp-file2)))))))


(defun full-test (input-filename type &optional inflate-type)
  ;; compress input-file to temp-file1, uncompress it back to temp-file2
  ;; and compare temp-file2 to input-filename, error if not same.
  (unless inflate-type
    (setq inflate-type type))
  (let (temp-file1 temp-file2)
    (unwind-protect
	(progn
	  (setq temp-file1 (sys:make-temp-file-name "full1"))
	  (setq temp-file2 (sys:make-temp-file-name "full2"))
	  (format t "; full test on ~a type ~s ~s~%"
		  (enough-namestring input-filename) type inflate-type)
	  (format t "  ; deflate ~a to ~a~%" temp-file1 temp-file2)
	  (deflate-file input-filename temp-file1 type)
	  (format t "  ; inflate ~a to ~a~%" temp-file1 temp-file2)
	  (inflate-file temp-file1 temp-file2 inflate-type)
	  ;;(format t "; compare ~a to ~a~%" input-filename temp-file2)
	  (test-t (excl::compare-files input-filename temp-file2)))
      (when temp-file1 (ignore-errors (delete-file temp-file1)))
      (when temp-file2 (ignore-errors (delete-file temp-file2))))))

;; skip the 2-byte zlib header
(defun custom-zlib-head (p)
  (read-byte p) (read-byte p)
  2)

;; skip the 4-byte zlib trailer
(defun custom-zlib-tail (p)
  (dotimes (i 4) (read-byte p))
  4)

(defun test-invalid-first-byte-in-header ()
  (let ((in-file "/dev/zero"))
    (dolist (type '(:gzip :zlib))
      (format t "~&Testing faulty header detection (~s)..." type)
      (with-open-file (in in-file :direction :input)
	(test-no-err (make-instance 'util.zip::inflate-stream
		       :compression type
		       :input-handle in)))
      (format t "okay.~%"))))

(defun test-gzip ()
  (map-over-directory
   (lambda (p)
     ;; Only check .cl files. This test file may be run in 
     ;; a directory with many large files resulting in
     ;; the test taking a _very_ long time.
     (when (string-equal "cl" (pathname-type p))
       (deflate-test p)
       (inflate-test p)
       (dolist (type '(:gzip :zlib :deflate nil))
	 (full-test p type))
       ;; test custom compression type.
       (full-test p :zlib '(custom-zlib-head custom-zlib-tail))
       ))
   "./"
   :recurse nil)
  
  (test-invalid-first-byte-in-header))


(defun test-bug25005 ()
  (flet ((gzip (data)
	   (let ((stream (make-instance 'util.zip:deflate-stream
			   :target (make-array 1000 :element-type '(unsigned-byte 8))
			   :compression :gzip)))
	     (write-sequence data stream)
	     (close stream)
	     (multiple-value-bind (result-vector len)
		 (util.zip:deflate-stream-vector-combined stream)
	       (subseq result-vector 0 len))))
	 (read-stream-to-bytearray (stream &aux (ret (make-array 
						      1000 :element-type '(unsigned-byte 8)
						      :adjustable t :fill-pointer 0))
						byte)
	   (loop 
	     (or (setq byte (read-byte stream nil nil))
		 (return ret))
	     (vector-push-extend byte ret)))
	 (collect-bytes (stream &aux ch bytes)
	   (loop
	     (or (setq ch (read-byte stream nil nil))
		 (return (concatenate 'string (reverse bytes))))
	     (push (code-char ch) bytes)))
	 )
    (let ((double-gzipped-text (gzip (gzip "hello"))))
      (format t "~&Testing double gzip via intermediate buffer...~%")
      (let* ((i1 (make-instance 'util.zip:inflate-stream
		   :compression :gzip
		   :input-handle (make-buffer-input-stream double-gzipped-text)))
	     (i2 (make-instance 'util.zip:inflate-stream
		   :compression :gzip
		   :input-handle (make-buffer-input-stream (read-stream-to-bytearray i1)))))
	(test "hello" (collect-bytes i2) :test #'equal :fail-info "with intermediate buffer"))
      (format t "~&Testing double gzip via nested streams...~%")
      (let* ((i1 (make-instance 'util.zip:inflate-stream
		   :compression :gzip
		   :input-handle (make-buffer-input-stream double-gzipped-text)))
	     (i2 (make-instance 'util.zip:inflate-stream
		   :compression :gzip
		   :input-handle i1)))
	(test "hello" (collect-bytes i2) :test #'equal :fail-info "nested inflate streams"))
      )))
    

(when *do-test* (do-test "gzip" #'test-zlib))
