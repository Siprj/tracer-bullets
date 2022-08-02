FROM haskell:9.2 as log-gen

RUN cabal update
WORKDIR /opt/app/
COPY ./haskell-file-elasticsearch.cabal /opt/app/
COPY ./cabal.project /opt/app/
RUN cabal build --only-dependencies -j

COPY . /opt/app/
RUN cabal install

CMD ["haskell-file-elasticsearch"]
