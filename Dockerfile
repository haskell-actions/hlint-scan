# syntax=docker/dockerfile:1

FROM haskell:9.2.7 AS build
RUN stack install hlint-scan:exe:hlint-scan hlint
COPY hlint /hlint
COPY hlint-scan /hlint-scan

FROM alpine:latest
COPY --from=build /hlint /hlint
COPY --from=build /hlint-scan /hlint-scan
ENTRYPOINT ["/hlint-scan"]
