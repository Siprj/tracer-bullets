FROM haskell:9.4.8-slim-buster AS build-haskell
WORKDIR /usr/src/app
RUN cabal update && cabal --version
COPY ./haskell-example.cabal ./haskell-example.cabal
RUN cabal build --only-dependencies -j4

COPY ./app ./app
RUN cabal install --install-method=copy --installdir=/usr/src/app/bin/

FROM debian:buster-slim AS haskell-example
WORKDIR /usr/src/app
COPY --from=build-haskell /usr/src/app/bin/haskell-example ./
CMD ["./haskell-example"]
