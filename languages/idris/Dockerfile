FROM alpine:latest

RUN echo "@testing http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && \
    apk update && \
    apk add idris@testing && \
    rm -rf /var/cache/apk/*

COPY run.sh /var/run/
