FROM alpine:latest as build

ENV LANG     en_US.UTF-8
ENV LANGUAGE en_US:en
ENV LC_ALL   en_US.UTF-8

WORKDIR /tmp/haskell
RUN apk update && \
    apk upgrade --available && \
    apk add \
        build-base make cmake gcc gmp curl xz perl cpio coreutils \
        binutils-gold tar gzip unzip \
        libc-dev musl-dev ncurses-dev gmp-dev zlib-dev expat-dev libffi-dev \
        gd-dev postgresql-dev linux-headers

RUN curl https://gitlab.haskell.org/haskell/ghcup-hs/raw/master/bootstrap-haskell -sSf | BOOTSTRAP_HASKELL_NONINTERACTIVE=1 sh && \
    /root/.ghcup/bin/ghcup set
ENV PATH "$PATH:/root/.cabal/bin:/root/.ghcup/bin"

WORKDIR /tmp/myriad
COPY . .
RUN cabal new-install

RUN mkdir -p /opt/myriad && \
    cp -L /root/.cabal/bin/myriad /opt/myriad && \
    mv languages /opt/myriad && \
    mv config.example.yaml /opt/myriad/config.yaml


FROM alpine:latest
RUN apk add --no-cache docker-cli gmp
WORKDIR /opt/myriad
COPY --from=build /opt/myriad .

EXPOSE 8081

ENTRYPOINT ["./myriad"]