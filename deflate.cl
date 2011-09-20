#+(version= 8 2)
(sys:defpatch "deflate" 3
  "v0: new deflate-stream;
v1: load zlib.so.1 instead of zlib.so;
v2: fix memory leak.
v3: Add support for creating :gzip, :zlib, or raw :deflates streams."
  :type :system
  :post-loadable t)

;; stream for doing compression
;;
;; code based on zlib.cl from AllegroGraph written by marijnh
;;
(defpackage :util.zip
  (:use :common-lisp :excl)
  (:export #:deflate-stream
	   #:deflate-target-stream
	   #:deflate-stream-vector
	   #:deflate-stream-vector-combined))

(in-package :util.zip)



(eval-when (compile load eval)
(defconstant *zlib-in-buffer-size* (* 16 1024))
(defconstant *zlib-out-buffer-size* (* 17 1024))
)




;; deflate-stream
;;
;; create via
;;   (make-instance 'deflate-stream :target stream-or-vector 
;;				 :compression ckind)
;;
;; The target argument is required. It says where to compressed
;; data.
;; The :compression argument is optional.  It can be :gzip,
;;   :zlib, or :deflate.  If not given :gzip is assumed
;;
;; If a stream is given as the :target then the compressed bytes 
;; are written to that stream as they are generated.
;; You cannot count on the zlib module to generate
;; compressed bytes immediately.  The only time you can be sure
;; that all the compressed bytes have been send to the stream
;; is after you close the deflate-stream.  After the deflate-stream
;; is closed, the last bits of compressed data is written to
;; the target stream and a force-output is done the target
;; stream.   The target stream is NOT closed.
;;
;; If the :target value is a simple vector of (unsigned-byte 8) then 
;; the compressed bytes are written to that vector.  If that
;; vector fills up then more vectors are allocated.
;; After the deflate-stream is closed you can call
;; deflate-stream-vector to retrieve all off the vectors that contain
;; the compressed data.  You can also call deflate-stream-vector-combined
;; to create a single vector containing all of the compressed data.
;;
;; 
;; examples
;;  (setq str (make-instance 'deflate-stream :target (make-array 1000 :element-type '(unsigned-byte 8))))
;;  (dotimes (i 1000) (write-byte (mod i 30) str))
;;  (close str)
;;  (deflate-stream-vector-combined str)
;;

(eval-when (compile load eval) (require :util-string))

(excl:without-package-locks
(defvar sys::*zlib-system-library*
    (excl::machine-case :host
      ((:msx86 :msx86-64)
       ;; I don't know of a source for a 64-bit version of this library,
       ;; but it would be called this if there were one.
       "zlib1.dll")
      ((:macosx86 :macosx86-64) "libz.1.dylib")
;;;; FreeBSD changes the name of this library more than other
;;;; platforms, which seem to keep it static between releases.
;;;; The values here are defined per ACL version, which seems the most
;;;; sensible way to do it.
      #+(version= 8 2) (:freebsd "libz.so.3")
      #+(version= 9 0) ((:freebsd :ipp3) "libz.so.5")
      (t (util.string:string+ "libz." sys::*dll-type* ".1"))))
)

(defvar *zlib-dll-loaded* nil)
(when (not *zlib-dll-loaded*)
  (handler-case (load sys::*zlib-system-library* :system-library t :foreign t)
    (error (c)
      (error "~
This Allegro CL module requires the compression library named libz ~
to be present for the deflate module to load properly.  ~
See http://zlib.net for versions for various platforms.  The ~
actual error:~%  ~a" c)))
	(setq *zlib-dll-loaded* t))


(pushnew :zlib-deflate *features*)
(provide :deflate)

(ff:def-foreign-type z-stream
    (:struct (next-in (* :void))  ; next input byte
             (avail-in :unsigned-int)    ; number of bytes available at next-in
             (total-in :unsigned-long)   ; total nb of input bytes read so far

             (next-out (* :void)) ; next output byte should be put there
             (avail-out :unsigned-int)   ; remaining free space at next_out
             (total-out :unsigned-long)  ; total nb of bytes output so far

             (msg (* :char))      ; last error message, NULL if no error
             (state (* :void))    ; not visible by applications

             (zalloc (* :void))   ; used to allocate the internal state
             (zfree (* :void))    ; used to free the internal state
             (opaque (* :void))   ; private data object passed to zalloc and zfree

             (data-type :int)    ; best guess about the data type: binary or text
             (adler :unsigned-long)      ; adler32 value of the uncompressed data
             (reserved :unsigned-long))) ; reserved for future use


(ff:def-foreign-type deflate-in-buffer
    (:struct (buff (:array :unsigned-char #.*zlib-in-buffer-size*))))

(ff:def-foreign-type deflate-out-buffer
    (:struct (buff (:array :unsigned-char #.*zlib-out-buffer-size*))))



(defmacro z-stream-slot (name obj)
  `(ff:fslot-value-typed 'z-stream :c ,obj ',name))

(ff:def-foreign-call (deflate-init-2 "deflateInit2_")
    ((stream (* z-stream))
     (level :int)
     (method :int)
     (window-bits :int)
     (mem-level :int)
     (strategy :int)
     (version (* :char))
     (stream-size :int))
  :strings-convert t
  :returning :int)

(ff:def-foreign-call (deflate "deflate")
    ((stream (* z-stream))
     (flush :int))
  :returning :int)

(ff:def-foreign-call (deflate-end "deflateEnd")
    ((stream (* z-stream)))
  :returning :int)


(def-stream-class deflate-stream (single-channel-simple-stream)
  ((z-state
    ;; malloc z-state foreign object
    ;; holding the info zlib needs to use to run
    :initform 0
    :accessor z-state)
   
   ; using existing slots
   ; from stream
   ;  flags
   ;  output-handle    - stream to vector
   ;  external-format
   ;
   ; from simple-stream
   ;   buffer    malloc,ed, contains user written data
   ;   buffer-ptr next byte to write
   ;   charpos    always nil since we don't track
   ;
   ;
   
   ; new slots

   (z-stream
    ;; holds malloc'ed zlib struct that controls compression
    :initform 0
    :accessor zlib-z-stream)

   (in-buffer
    ;; malloced buffer to which data is copied before compression
    ;; since the compressor requires a static buffer
    :accessor zlib-in-buffer)
   
   
   (z-buffer
    ;; malloc buffer holding data after compression
    ;; it's malloced so it stays still
    
    :initform 0
    :accessor zlib-z-buffer)
   
   
   (in-buffer-ptr  :initform 0
		   :accessor zlib-in-buffer-ptr)
   

   ; points to the lispstatic-reclaimable resources for
   ; this stream. Should the stream be dropped and never
   ; closed this list will be gc'ed and that will the
   ; allow the static data to be reclaimed.
   (static-resources :initform nil
		     :accessor zlib-static-resources)
   
   ; trace usage
   (in-bytes :initform 0
	     :accessor zlib-in-bytes)
   
   (out-bytes :initform 0
	      :accessor zlib-out-bytes)
   
   
   ;; for stream target
   (target-stream  
    :initform nil
    :accessor deflate-target-stream)
    
    ;; for vector target
   (target-vector 
    :initform nil
    :accessor zlib-target-vector)
   
   (target-vector-pos
    :initform 0
    :accessor zlib-target-vector-pos)
   
   (target-vector-old
    ; list of full previous target vectors
    :initform nil
    :accessor zlib-target-vector-old)
   
   ;; end vector target
		  
   
   )
   )

(defmethod print-object ((p deflate-stream) s)
  (print-unreadable-object (p s :identity t :type t)
    (format s "in ~d / out ~d" (zlib-in-bytes p) (zlib-out-bytes p))))

(defmethod device-open  ((p deflate-stream) dummy options)
  (declare (ignore dummy))
  
  
  (let ((output-target (getf options :target))
	(compression   (or (getf options :compression)
			   :gzip))
	(static-resources (get-deflate-buffer-resources)))

    (setf (zlib-static-resources p) static-resources)

    (destructuring-bind (z-stream-vec in-buffer-vec out-buffer-vec) 
	static-resources
      
      
      (typecase output-target
	(stream
	 (setf (deflate-target-stream p) output-target))
	((simple-array (unsigned-byte 8) (*))
	 (setf (zlib-target-vector p) output-target))
	(t (error "the value of initarg :target must be a stream or simple (unsigned-byte 8) vector, not ~s" output-target)))
    
      (if* (not (member compression '(:gzip :zlib :deflate)))
	 then (error "compression must be :gzip, :zlib, or :deflate, not ~s"
		     compression))
      
      (if* (null output-target)
	 then (error  ":output-target must be given when creating a deflate-stream"))

      ;; normal these would be written using the with-stream-class
      ;; macro and sm, but we may want to open source this so best
      ;; to write it in code that doesn't need a dcl to build
      (setf 
	  (slot-value p 'excl::buffer) (make-array 4096 :element-type '(unsigned-byte 8))
	  (zlib-in-buffer  p)  (ff:fslot-address-typed 
				'deflate-in-buffer
				:foreign-static-gc
				in-buffer-vec)
				
						       
	  (zlib-z-buffer   p)  (ff:fslot-address-typed 
				'deflate-out-buffer
				:foreign-static-gc
				out-buffer-vec)
	  
	  (slot-value p 'excl::buffer-ptr) 0
	
	  (zlib-z-stream p) (make-z-stream (ff:fslot-address-typed
					    'z-stream
					    :foreign-static-gc
					    z-stream-vec)
					   
					   compression)
	
	  (slot-value p 'excl::control-out) excl::*std-control-out-table*
	
	  )
    
      ; does some kind of initialization I think
      (setf (stream-external-format p)
	(stream-external-format p))
    
      (add-stream-instance-flags p :output :simple)
    
      t)))



(defun make-z-stream (z-stream type)
  (let (
	;; windowBits default value is 15 for zlib header and trailer
	;; if you add 16 you get gzip header and trailer
	;; if windowBits is -15, then you get a raw deflate stream.
        (window-bits (+ 15 (ecase type (:gzip 16) (:zlib 0) (:deflate -30)))))
    (setf (z-stream-slot zalloc z-stream) 0
          (z-stream-slot zfree z-stream) 0
          (z-stream-slot opaque z-stream) 0)
    (let ((err (deflate-init-2 z-stream 
		   -1 #|default level|#
		   8 #|Z_DEFLATED|# 
		   window-bits
		   8 #|default level|#
		   0 #|Z_DEFAULT_STRATEGY|# 
		   "1.2.3.4" #|version|#
		   (ff:sizeof-fobject 'z-stream))))
      (if* (< err 0 #|Z_OK|#) 
	      then (error "deflateInit2_ returned ~a" err)))
    z-stream))

(defun finish-z-stream (z-stream)
  ;; free C resources controlled by zlib
  (deflate-end z-stream))

   

(defmethod device-write ((p deflate-stream) buffer start end blocking)
  ;;
  ;; buffer is an ausb8 
  ;; 
  ;; fill up the internal static buffer 
  ;; do the compressing should the buffer fill up
  ;;
  (declare (ignore blocking))
  
  (let ((in-buffer (zlib-in-buffer p))
	(in-buffer-ptr (zlib-in-buffer-ptr p))
	(max *zlib-in-buffer-size*)
	(buffer (or buffer (slot-value p 'excl::buffer)))
	)
    
    
    (do ((i start (1+ i)))
	((>= i end))
      
      
      (setf (sys::memref-int in-buffer in-buffer-ptr 0 :unsigned-byte) 
	(aref buffer i))
      (incf in-buffer-ptr)

      (if* (>= in-buffer-ptr max)
	 then ; must flush the buffer
	      (setf (zlib-in-buffer-ptr p) in-buffer-ptr)
	      (flush-deflate-stream-input-buffer p)
	      (setq in-buffer-ptr (zlib-in-buffer-ptr p))))
    
    
    (setf (zlib-in-buffer-ptr p) in-buffer-ptr)
    
    
    end))


(defmethod flush-deflate-stream-input-buffer ((p deflate-stream))
  ;; compress the contents of the input buffer
  
  (let ((z-stream  (zlib-z-stream p)))
    
    (setf (z-stream-slot avail-in z-stream) (zlib-in-buffer-ptr p)
	  (z-stream-slot next-in  z-stream) (zlib-in-buffer     p))

    (incf (zlib-in-bytes p) (zlib-in-buffer-ptr p))
    
    (setf (zlib-in-buffer-ptr p) 0)
    
    (loop
      (if* (zerop (z-stream-slot avail-in z-stream))
	 then ; no more to compress
	      (return))
      
	      
      (setf (z-stream-slot next-out z-stream) (zlib-z-buffer p)
	    (z-stream-slot avail-out z-stream) *zlib-out-buffer-size*)
      
      (let ((error (deflate z-stream 0 ; Z_NO_FLUSH
		     )))
	
	(if* (< error 0)
	   then (error "zlib's deflate returned error code ~s" error))
	
	
	(process-compressed-result p)))))

(defmethod finish-zlib-compression ((p deflate-stream))
  ;; finish the compression of the contents of the input buffer


  (flush-deflate-stream-input-buffer p)
  
  (let ((z-stream  (zlib-z-stream p)))
    

    
    (loop
      (setf (z-stream-slot next-out z-stream) (zlib-z-buffer p)
	    (z-stream-slot avail-out z-stream) *zlib-out-buffer-size*)
      
      (let ((error (deflate z-stream 4 ; Z_FINISH
		     )))
	
	(process-compressed-result p)
	(if* (eq error 1) ; Z_STREAM_END
	   then (return))
	))))
      

(defmethod process-compressed-result ((p deflate-stream))
  ;; take the resulant compressed bytes and put 
  ;; them somewhere
  
  (let ((static-vec (zlib-z-buffer p))
	(bytes      (- *zlib-out-buffer-size*
		       (z-stream-slot avail-out (zlib-z-stream p)))))
    

    (incf (zlib-out-bytes p) bytes)
    ; we'll just write byte all the values

    (let ((target-stream (deflate-target-stream p)))
      (if* target-stream
	 then 
	      (dotimes (i bytes)
		(write-byte (sys:memref-int static-vec i 0 :unsigned-byte) 
			    target-stream))
	 else (let* ((vec (zlib-target-vector p))
		     (pos (zlib-target-vector-pos p))
		     (max (length vec))
		     (static-base 0))

		(loop
		  (let ((docopy (min bytes (- max pos))))
		
		    (dotimes (i docopy)
		      (setf (aref vec (+ pos i)) 
			(sys:memref-int static-vec i static-base :unsigned-byte)))
		    (if* (> bytes docopy)
		       then ; we overflowed, more to do
			    (push vec (zlib-target-vector-old p))
			    (setq vec (make-array (length vec) 
						  :element-type 
						  '(unsigned-byte 8)))
			    (setf (zlib-target-vector p) vec)
			    
			    (setq pos 0)
			    (incf static-base docopy)
			    (decf bytes docopy)
			    
		       else ; finished
			    (setf (zlib-target-vector-pos p) (+ pos docopy))
			    (return)))))))))
		
		



(defmethod device-close ((p deflate-stream) abort)
  
  (if* (not abort)
     then ; flush all current data
	  (finish-zlib-compression p))

  (free-deflate-buffer-resource (zlib-static-resources p))

  (let ((z-stream (zlib-z-stream p)))
    (if* (not (zerop z-stream))
       then (finish-z-stream  z-stream))
    (setf (zlib-z-stream p) 0))
  
  (if* (deflate-target-stream p) 
     then (force-output (deflate-target-stream p)))
  p
  )

(without-package-locks
(defmethod excl::inner-stream ((p deflate-stream))
  (deflate-target-stream p)))
  

(defmethod deflate-stream-vector ((p deflate-stream))
  (let ((vec (zlib-target-vector p)))
    (if* vec
       then (values vec 
		    (zlib-target-vector-pos p)
		    (zlib-target-vector-old p))
       else (error "deflate-stream ~s was not created with a vector target" p))))

(defmethod deflate-stream-vector-combined ((p deflate-stream))
  (multiple-value-bind (last pos old) (deflate-stream-vector p)
    (if* old
       then ; must combine
	    (let ((size pos))
	      (dolist (v old) (incf size (length v)))
	      (let ((ans (make-array size :element-type '(unsigned-byte 8)))
		    (start 0))
		(dolist (v (reverse old))
		  (replace ans v :start1 start)
		  (incf start (length v)))
		(replace  ans last :start1 start :end2 pos)
		(values ans size)))
       else (values last pos))))

		    



;; we'll resource the buffers we need to speed up allocation

(defvar *deflate-resource-lock* (mp:make-process-lock))

(defvar *deflate-malloc-resources* nil)



(defun get-deflate-buffer-resources ()
  (mp:with-process-lock (*deflate-resource-lock*)
    (let ((buffers (pop *deflate-malloc-resources*)))
      (if* buffers
	 thenret
	 else (list (ff:allocate-fobject 'z-stream :foreign-static-gc)
		    (ff:allocate-fobject 'deflate-in-buffer :foreign-static-gc)
		    (ff:allocate-fobject 'deflate-out-buffer :foreign-static-gc))))))
			 

(defun free-deflate-buffer-resource (buffers)
  (mp:with-process-lock (*deflate-resource-lock*)
    (push buffers *deflate-malloc-resources*)))

		      

