FROM debian:stable-slim
LABEL author="1Computer1"
ENV LANG C.UTF-8

RUN apt-get update && \
    apt-get install -y --no-install-recommends gnupg dirmngr ca-certificates && \
    echo 'deb https://downloads.haskell.org/debian stretch main' > /etc/apt/sources.list.d/ghc.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA3CBA3FFE22B574 && \
    apt-get update && \
    apt-get install -y --no-install-recommends ghc-9.0.1 && \
    apt-get purge -y gnupg dirmngr ca-certificates && \
    apt-get autoremove -y && \
    apt-get autoclean -y

ENV PATH /opt/ghc/9.0.1/bin:$PATH

COPY run.sh /var/run/
