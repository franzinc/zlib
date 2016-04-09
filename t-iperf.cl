;; See the file LICENSE for the full license governing this code.

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
