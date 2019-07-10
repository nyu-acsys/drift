#!/bin/bash

set -e

DIRS="-Is src/utils,src/domain,src/main,src/algo,src/frontends"
TARGET="src/main/tests "
APRONPKG="apron,apron.boxD,apron.boxMPFR,apron.boxMPQ,apron.octD,apron.octMPQ,apron.polkaGrid,apron.polkaMPQ,apron.polkaRll"
FLAGS="-cflag -g -lflag -g -libs str -pkgs $APRONPKG $DIRS"
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
