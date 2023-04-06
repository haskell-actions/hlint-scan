# syntax=docker/dockerfile:1

FROM haskell:9.4.4 AS build
RUN mkdir -p /src
WORKDIR /src
RUN git clone https://github.com/haskell-actions/hlint-scan.git
WORKDIR /src/hlint-scan
RUN git checkout haskell
RUN stack install hlint hlint-scan:exe:hlint-scan
RUN cp $(stack path --local-bin)/hlint /
RUN cp $(stack path --local-bin)/hlint-scan /

FROM haskell:9.4.4-slim
COPY --from=build /hlint /hlint-scan /
ENTRYPOINT ["/hlint-scan"]
