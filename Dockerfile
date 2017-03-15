# Based on https://www.fpcomplete.com/blog/2016/10/static-compilation-with-stack
# USE ALPINE LINUX
FROM alpine:edge
RUN echo '@testing http://dl-cdn.alpinelinux.org/alpine/edge/testing' >> /etc/apk/repositories
RUN apk update

# We add vim to get xxd
RUN apk add \
    alpine-sdk \
    bash \
    ca-certificates \
    file \
    bzip2-dev \
    cabal@testing \
    ghc@testing \
    ghc-dev@testing \
    libffi-dev \
    git \
    gmp-dev \
    m4 \
    make \
    vim \
    xz \
    zlib-dev

# GRAB A RECENT BINARY OF STACK
RUN wget -qO- https://get.haskellstack.org/ | sh

RUN chmod 755 /usr/local/bin/stack
RUN mkdir -p /usr/src/

RUN git clone --depth=10 https://github.com/luispedro/ngless  /usr/src/ngless
WORKDIR /usr/src/ngless
RUN m4 NGLess.cabal.m4 > NGLess.cabal
RUN stack --local-bin-path /usr/local/bin install  --system-ghc --ghc-options '-optl-static -fPIC'
