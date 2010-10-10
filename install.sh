#!/bin/bash
set -e
if [ $# != 1 ]; then
   echo "usage: ./install.sh destdir"
   echo
   echo "Installs ERT into an Emacs bzr workspace.  The argument destdir must"
   echo "be the root of that workspace."
   exit 1
fi
destdir="$1"
origdir="$(pwd)"

lispfiles="$(find lisp -name \*.el)"
testfiles="$(find test -name \*.el -o -name Makefile.in)"
docfiles="$(find doc -name \*.texi)"

cd -- "$destdir"

if ! bzr diff -q > /dev/null; then
    echo "There are pending changes in $(pwd).  Aborting."
    exit 1
fi

mkdir -p test/automated

for i in $lispfiles $testfiles $docfiles; do
    sed -e 's/NOT \(part of GNU Emacs\)/\1/' < "$origdir/$i" > "$i"
done
bzr add $lispfiles $testfiles $docfiles

patch -p0 < "$origdir/install.patch"

autoreconf
