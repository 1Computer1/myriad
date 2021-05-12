FROM alpine as build

COPY bf.cpp .
RUN apk add --no-cache g++ && \
    g++ bf.cpp -o bf

FROM alpine
LABEL author="1Computer1"

RUN apk add --no-cache libstdc++
COPY --from=build bf /usr/local/bin/
COPY run.sh /var/run/
