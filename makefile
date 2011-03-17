# On Windows, this makefile requires the use of GNU make from Redhat
# (http://sources.redhat.com/cygwin/).

SHELL = sh

on_windows = $(shell if test -d "c:/"; then echo yes; else echo no; fi)

use_dcl = $(shell if test -f ../dcl.dxl; then echo yes; else echo no; fi)

ifeq ($(on_windows),yes)
plus_args = +M +B +cn
endif

ifeq ($(use_dcl),yes)
mlisp = ../lisp $(plus_args) -I dcl.dxl
endif

ifndef mlisp
ifeq ($(on_windows),yes)
mlisp = "/cygdrive/c/acl82/mlisp.exe" $(plus_args) 
else
mlisp = /fi/cl/8.2/bin/mlisp
endif
endif

test: FORCE
	rm -f build.tmp
	echo '(setq excl::*break-on-warnings* t)' >> build.tmp
	echo '(load (compile-file "inflate.cl"))' >> build.tmp
	echo '(load (compile-file "deflate.cl"))' >> build.tmp
	echo '(dribble "test.out")' >> build.tmp
	echo '(time (load (compile-file "t-gzip.cl")))' >> build.tmp
	echo '(exit (+ test::.total-errors. test::*test-unexpected-failures* util.test::*test-errors*))' >> build.tmp
# -batch must come before -L, since arguments are evaluated from left to right
	$(mlisp) -batch -L build.tmp -kill

clean:	FORCE
	rm -f build.tmp
	find . -name '*.fasl' -print | xargs rm -f

FORCE:
