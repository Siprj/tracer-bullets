FROM haskell:9.4.8-slim-buster AS build-haskell-otel
WORKDIR /usr/src/app
RUN cabal update && cabal --version
COPY ./haskell-otel-example.cabal ./haskell-otel-example.cabal
COPY ./cabal.project ./cabal.project
RUN cabal build --only-dependencies -j4

COPY ./app ./app
RUN cabal install --install-method=copy --installdir=/usr/src/app/bin/

FROM debian:buster-slim AS haskell-otel-example
WORKDIR /usr/src/app
COPY --from=build-haskell-otel /usr/src/app/bin/haskell-otel-example ./
CMD ["./haskell-otel-example"]
