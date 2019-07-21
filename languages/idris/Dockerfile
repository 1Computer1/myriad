FROM alpine:stable

RUN echo "@testing http://nl.alpinelinux.org/alpine/edge/testing" >> /etc/apk/repositories && \
    apk update && \
    apk add idris@testing

COPY run.sh /var/run/
