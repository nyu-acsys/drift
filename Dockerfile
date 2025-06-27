# Base image
FROM ubuntu:22.04

# Create a user

ARG USERNAME=evdrift
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN apt-get update -y

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME --shell /bin/zsh \
    #
    # [Optional] Add sudo support. 
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# Install dependencies
RUN apt-get update -y && apt-get install -y \
  git \
  curl \
  build-essential \
  ca-certificates \
  libppl-dev \
  libblas-dev \
  liblapack-dev \
  libmpfr-dev \
  libgmp-dev \
  libglpk-dev \
  libffi-dev \
  opam \
  autoconf libpcre2-dev pkg-config python3-distutils xdot \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* 

# install Rust
RUN curl https://sh.rustup.rs -sSf | sh -s -- -y
# Add Cargo (Rust package manager) to PATH
ENV PATH="/root/.cargo/bin:${PATH}"

# Project's directory
RUN mkdir /oopsla25
WORKDIR /oopsla25

# Set up OCaml env (required by Drift) 
RUN opam init -y --disable-sandboxing --compiler 4.11.1
RUN opam switch 4.11.1

# Set environment variables
RUN export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$OPAM_SWITCH_PREFIX/share/apron/lib"
# ENV LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$OPAM_SWITCH_PREFIX/share/apron/lib" 

# Drift source code and compilation
RUN git clone https://github.com/nyu-acsys/drift.git
WORKDIR /oopsla25/drift

# Drift executable
RUN opam install -y dune \
  && eval $(opam env) \
  && opam install . --deps -y \
  && dune build

# MoCHi 
WORKDIR /oopsla25
RUN git clone https://github.com/hopv/MoCHi.git
WORKDIR /oopsla25/MoCHi
RUN opam switch create 4.14.0
# Dep: HorSat2
WORKDIR /oopsla25
RUN git clone https://github.com/hopv/horsat2.git
WORKDIR /oopsla25/horsat2
RUN eval $(opam env) && make horsat2
ENV PATH="/oopsla25/horsat2:${PATH}"
# Dep: Hoice (installed via Cargo)
RUN rustup update stable
RUN cargo install --git https://github.com/hopv/hoice
# MoCHi executable
WORKDIR /oopsla25/MoCHi
RUN opam install -y dune \
  && eval $(opam env) \
  && dune build mochi.opam \
  && opam install . --deps-only -y \
  && dune build --release

# RCaml
WORKDIR /oopsla25
RUN git clone https://github.com/hiroshi-unno/coar.git
WORKDIR /oopsla25/coar
RUN opam switch create 5.3.0
RUN opam install -y dune \
  && eval $(opam env) \
  && opam install . --deps-only -y \
  && dune build main.exe

# Symbolic links needed for running the tools
WORKDIR /oopsla25/drift
RUN ln -s /oopsla25/MoCHi/src/mochi.exe mochi.exe
RUN ln -s /oopsla25/coar/_build/default/main.exe rcaml.exe
RUN ln -s /oopsla25/coar/config config

 
