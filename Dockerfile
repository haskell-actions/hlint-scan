# syntax=docker/dockerfile:1

# We fetch and build a specific unreleased version of HLint as well,
# since a version of HLint with SARIF support has not been released yet.
#
# Once one has been, we may either continue to bundle the hlint binary
# together but at a more stable and official set of versions.
# Alternatively, we could have the action retrieve an hlint release
# automatically if one is not already available locally in the action.

FROM haskell:9.4.4@sha256:317a94164894807ea3fe53d66d42cc13ad52487d03388b10022de24dcaf83725 AS build
RUN mkdir -p /src
WORKDIR /src
RUN git clone https://github.com/haskell-actions/hlint-scan.git
WORKDIR /src/hlint-scan
RUN stack install hlint hlint-scan:exe:hlint-scan
RUN cp $(stack path --local-bin)/hlint /
RUN cp $(stack path --local-bin)/hlint-scan /

FROM haskell:9.4.4-slim@sha256:d7b0fa17fd77c70a14a23f4a0bdb1d6ef712815c9d369ac6726663110659a56f
COPY --from=build /hlint /hlint-scan /
ENTRYPOINT ["/hlint-scan"]
