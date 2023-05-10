#!/bin/bash

set -e

DIRS="-Is src/utils,src/domain,src/main,src/algo,src/frontends,src/effects"
TARGET="src/main/drift "
APRONPKG="apron,apron.boxD,apron.boxMPFR,apron.boxMPQ,apron.octD,apron.octMPQ,apron.polkaMPQ,apron.polkaRll,\
apron.ppl,apron.t1pD,apron.t1pMPFR,apron.t1pMPQ,apron.polkaGrid"
FLAGS="-cflags -g,-rectypes -lflags -g -pkgs $APRONPKG,symkat,batteries,ocolor $DIRS"
OCAMLBUILD=ocamlbuild
MENHIR=menhir

eval `opam config env`

ocb()
{
    $OCAMLBUILD -use-ocamlfind $FLAGS $*
}

if [ $# -eq 0 ]; then
    action=native
else
    action=$1;
    shift
fi

case $action in
    clean)  ocb -clean;;
    native) ocb ${TARGET//" "/".native "} ;;
    byte)   ocb ${TARGET//" "/".byte "} ;;
    byte-debug)   ocb ${TARGET//" "/".d.byte "} ;;
    all)    ocb ${TARGET//" "/".native "} ${TARGET//" "/".byte "} ;;
    prof)   ocb ${TARGET//" "/".p.native "} ;;
    *)      echo "Unknown action $action";;
esac;

# ./build.sh
# profiling: ./build.sh prof
# run analysis: ./tests.p.native <-file> > out_idc
# check calls: gprof tests.p.native gmon.out > out_count
