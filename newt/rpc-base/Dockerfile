FROM ocaml/opam:alpine-3.6_ocaml-4.04.2_flambda
MAINTAINER John Smith

USER opam

RUN sudo apk update && \
    sudo apk add m4 unzip
RUN opam install -y thrift jbuilder base64 bitstring

# automatic run depext

RUN mkdir /home/opam/service
WORKDIR /home/opam/service

COPY rpc_server.ml newt.thrift jbuild /home/opam/service/

ENV GIST d20a8dc0ebe66412989406b6bce39787
RUN wget https://gist.github.com/$GIST/download -O gist.zip \
    && unzip -o -j gist.zip -d . \
    && rm gist.zip

RUN eval `opam config env` \
    && thrift -r -o . --gen ocaml newt.thrift \
    && jbuilder build rpc_server.exe

ENV ENTRY_FILE "splus.ml" #user input
ENV ENTRY_FUNC "plus"     #user input
CMD ["./_build/default/rpc_server.exe"]

# docker build -t matrixanger/add_service_rpc .
# docker run --name plus_service_rpc -p 8889:8889 -d matrixanger/add_service_rpc
# exec "ocaml_query_processor_rpc.ml"'
