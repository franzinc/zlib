#! /bin/bash

set -eu

cd zlib-1.2.11

# The following is for
#   NMAKE : fatal error U1065: invalid option '-'
MAKEFLAGS=

export PATH=${MSVCDIR}:$PATH
nmake -f win32/Makefile.msc clean
nmake -f win32/Makefile.msc 

cp zlib1.dll ..
