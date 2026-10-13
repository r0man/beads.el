#!/bin/sh
# autogen.sh --- bootstrap the beads.el autotools build.
#
# Run this once after a fresh checkout (it is not needed for a release
# tarball, which already ships ./configure and Makefile.in):
#
#   ./autogen.sh && ./configure && make
#
# It regenerates configure, Makefile.in, aclocal.m4 and the Automake
# helper scripts in build-aux/.

set -eu

srcdir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
cd "$srcdir"

exec autoreconf -fi
