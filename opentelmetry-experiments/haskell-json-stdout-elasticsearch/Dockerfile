FROM haskell:9.2.4 as log-gen

RUN cabal update
WORKDIR /opt/app/
COPY ./haskell-json-stdout-elasticsearch.cabal /opt/app/
COPY ./cabal.project /opt/app/
RUN cabal build --only-dependencies -j

COPY . /opt/app/
RUN cabal install

CMD ["haskell-json-stdout-elasticsearch"]
