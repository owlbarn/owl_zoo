
FROM matrixanger/zoo-base
MAINTAINER John Smith

RUN opam install -y lwt cohttp cohttp-lwt-unix yojson jbuilder

RUN apt-get update -y \
    && apt-get -y install wget imagemagick

RUN mkdir /service
WORKDIR /service

COPY server.ml jbuild /service/

RUN owl -run df8a0359cb9dd6528643b456ff7d7f3e \
    && find ~/.owl/zoo -iname '*' -exec cp \{\} . \; \
    && find . -name "*.ml" -exec sed -i '/^#/d' \{\} \;

RUN eval `opam config env` && jbuilder build server.bc

ENTRYPOINT ["./_build/default/server.bc"]
