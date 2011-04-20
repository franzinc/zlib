;; copyright (c) 2011 Franz Inc, Oakland, CA - All rights reserved.
;;
;; The software, data and information contained herein are proprietary
;; to, and comprise valuable trade secrets of, Franz, Inc.  They are
;; given in confidence by Franz, Inc. pursuant to a written license
;; agreement, and may be stored and used only in accordance with the terms
;; of such license.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in
;; DOD FAR Supplement 52.227-7013 (c) (1) (ii), as applicable.

(eval-when (compile load eval)
  (require :deflate)
  (require :test))

(in-package :test)

(defun deflate-file (input-filename output-filename)
  (with-open-file (in input-filename :direction :input)
    (with-open-file (out output-filename 
		     :direction :output
		     :if-exists :supersede)
      (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8)))
	    (deflate (make-instance 'util.zip::deflate-stream 
		       :target out
		       :compression :gzip)))
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

(defun inflate-file (input-filename output-filename)
  (with-open-file (in input-filename :direction :input)
    (with-open-file (out output-filename 
		     :direction :output
		     :if-exists :supersede)
      (let ((inflate (make-instance 'util.zip::inflate-stream
		       :input-handle in
		       :skip-gzip-header t))
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

(defun test-gzip ()
  (map-over-directory
   (lambda (p)
     ;; Don't check .out files, since the output of the tests themselves
     ;; might be going to one, and that means the files would be changing
     ;; and the tests will fail.
     (when (not (string-equal "out" (pathname-type p)))
       (deflate-test p)
       (inflate-test p)))
   "./"
   :recurse nil))

(when *do-test* (do-test "gzip" #'test-gzip))
