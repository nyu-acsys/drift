#!/bin/bash

set -e

DIRS="-Is src/utils,src/domain,src/main,src/algo,src/frontends"
TARGET="src/main/tests "
APRONPKG="apron,apron.boxD,apron.boxMPFR,apron.boxMPQ,apron.octD,apron.octMPQ,apron.polkaMPQ,apron.polkaRll,\
apron.ppl,apron.t1pD,apron.t1pMPFR,apron.t1pMPQ"
FLAGS="-cflags -g -lflags -g,-p -libs str -pkgs $APRONPKG $DIRS"
OCAMLBUILD=ocamlbuild
MENHIR=menhir


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
    all)    ocb ${TARGET//" "/".native "} ${TARGET//" "/".byte "} ;;
    prof)   ocb ${TARGET//" "/".p.native "} ;;
    *)      echo "Unknown action $action";;
esac;

# ./build.sh
# profiling: ./build.sh prof
# run analysis: ./tests.p.native <-file> > out_idc
# check calls: gprof tests.p.native gmon.out > out_count