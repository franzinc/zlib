;; copyright (c) 2011-2013 Franz Inc, Oakland, CA - All rights reserved.
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

(in-package :user)

(eval-when (compile load eval)
  (require :inflate "./inflate.fasl")
  (use-package :util.zip))

(defun test-inflate-1 ()
  (with-open-file (p "foo.n.gz")
    (skip-gzip-header p)
    (let ((comp (make-instance 'inflate-stream :input-handle p)))
      ;; inflate-stream, testing performance.
      ;; after, compare that the inflated stream is the same as the orig.
      (with-open-file (of "foo.n" :direction :output :if-exists :supersede)
	(let (byte)
	  (while (setq byte (read-byte comp nil nil))
	    (write-byte byte of)))))))

(defun test-inflate-2 ()
  (with-open-file (p "foo.n.gz")
    (let ((comp (make-instance 'inflate-stream :input-handle p
			       :skip-gzip-header t)))
      ;; inflate-stream, testing performance.
      ;; after, compare that the inflated stream is the same as the orig.
      (with-open-file (of "foo.n" :direction :output :if-exists :supersede)
	(let (byte)
	  (while (setq byte (read-byte comp nil nil))
	    (write-byte byte of)))))))

(defun test-inflate (&optional (count 10))
  (declare (ignorable i))
  (time (map-over-directory (lambda (p)
			      (when (not (string-equal "out" (pathname-type p)))
				(dotimes (i count)
				  (run-shell-command (format nil "gzip -c ~a > foo.n.gz" p))
				  (test-inflate-1)
				  (test-inflate-2))))
			    "./" :recurse nil))
  (delete-file "foo.n.gz"))
