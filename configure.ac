# Originally from https://github.com/chetant/LibClang
# Copyright (c) 2011 Chetan Taralekar
# 
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
# 
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
# 
#     * Neither the name of Chetan Taralekar nor the names of other
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

AC_INIT([documentalist], [0.1])
AC_CONFIG_SRCDIR([Text/Documentalist.hs])

AC_ARG_WITH([llvm-config], [AS_HELP_STRING([--with-llvm-config],[configure LLVM libraries with llvm-config @<:@default: yes@:>@])], [llvm_config=${withval}], [llvm_config=yes])
AC_CHECK_PROG([llvm_config_found], [llvm-config], [yes], [no])
if test "$llvm_config" = yes
then
	if test "$llvm_config_found" = no
	then
		AC_MSG_ERROR([
	-------------------------------------------------
	llvm-config not found.
	if llvm-config is present, ensure it is available
	on path (export PATH=...).
	NOTE: if file is present and configured,
	try running running "cabal build -vv" or 
	           "runhaskell Setup.hs build -vv"
	to get the exact commands run
	-------------------------------------------------])
	else
		# get relevant libs from llvm-config
		CFLAGS="$CFLAGS `llvm-config --cflags`"
		LDFLAGS="$LDFLAGS `llvm-config --ldflags`"
	fi
fi

AC_CHECK_HEADERS([clang-c/Index.h], [], [no_clang_include=yes])
if test "$no_clang_include" = yes
then
	AC_MSG_ERROR([
	-------------------------------------------------
	Cannot find <clang-c/Index.h>
	Please ensure libClang is installed and 
	is in CFLAGS (-I...) or passed through configure
	NOTE: if file is present and configured,
	try running running "cabal build -vv" or 
	           "runhaskell Setup.hs build -vv"
	to get the exact commands run
	-------------------------------------------------])
fi

AC_ARG_ENABLE([llvm-shared],[AS_HELP_STRING([--enable-llvm-shared=LIBSUFFIX],[build against LLVM shared libraries with libsuffix])],[llvm_shared=$enableval])
if test "$llvm_shared" != ""
then
	AC_CHECK_LIB([${llvm_shared}], [LLVMContextCreate], [llvm_shared_lib_found=yes], [llvm_shared_lib_found=no])
	if test "$llvm_shared_lib_found" = no
	then
		AC_MSG_ERROR([
	-------------------------------------------------
	llvm shared library ${llvm_shared} not found.
	if the library is present, ensure it is available
	on the lib search path (export LDFLAGS=...)
	-------------------------------------------------])
	else
		LDFLAGS="$LDFLAGS -l${llvm_shared}"
		AC_TRY_RUN([int main(int argc, int argv[]){LLVMContextCreate();return 0;}], [llvm_shared_lib_present=yes], [llvm_shared_lib_present=no])
		if test "$llvm_shared_lib_present" = no
		then
		AC_MSG_ERROR([
	-------------------------------------------------------
	specified llvm shared library ${llvm_shared} not found.
	if the library is present, ensure it is available
	on the lib search path (export LID_LIBRARY_PATH=...)
	-------------------------------------------------------])
		fi
	fi
else
	if test "$llvm_config_found" = yes
	then
		LDFLAGS="$LDFLAGS `llvm-config --libs`"
	fi
fi

AC_CHECK_LIB([clang], [clang_getClangVersion], [clang_lib_found=yes], [clang_lib_found=no])
if test "$clang_lib_found" = no
then
	AC_MSG_ERROR([
-------------------------------------------------
clang library not found.
if the library is present, ensure it is available
on the lib search path (export LDFLAGS=...)
-------------------------------------------------])
else
	LDFLAGS="$LDFLAGS -lclang"
fi

# Check for stdlib.h and inttypes
if test "$ac_cv_header_stdlib_h" != yes
then
	AC_MSG_ERROR([Cannot find stdlib.h])
fi
if test "$ac_cv_header_inttypes_h" != yes
then
	AC_MSG_ERROR([Cannot find inttypes.h])
fi

AC_OUTPUT
