FROM erickoskinen/drift-core:test1

COPY . /drift

WORKDIR /drift

RUN \
    eval `opam config env` && \
    opam install -qy batteries symkat && \
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$OPAM_SWITCH_PREFIX/share/apron/lib && \
    ./build.sh

# docker build -t erickoskinen/drift:test1 -f Dockerfile .
# docker run -it erickoskinen/drift:test1 bash
