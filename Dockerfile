# syntax=docker/dockerfile:1

# We fetch and build a specific unreleased version of HLint as well,
# since a version of HLint with SARIF support has not been released yet.
#
# Once one has been, we may either continue to bundle the hlint binary
# together but at a more stable and official set of versions.
# Alternatively, we could have the action retrieve an hlint release
# automatically if one is not already available locally in the action.

FROM haskell:9.6.2-buster@sha256:573246db240e413cf55d70c5009d53cfc9d05db29b5eae54a0906ae01b1fcf38 AS build
RUN git clone https://github.com/haskell-actions/hlint-scan.git /src/hlint-scan
WORKDIR /src/hlint-scan
RUN stack install hlint hlint-scan:exe:hlint-scan && \
    cp "$(stack path --local-bin)/hlint" "$(stack path --local-bin)/hlint-scan" /

FROM debian:buster-slim@sha256:eee33fc4994a42efcdd2fde591a82e2de62fa1a08c87c2508ef60cb4af438776
RUN apt-get --yes update && \
    apt-get --yes --no-install-recommends install ca-certificates=20200601~deb10u2 && \
    apt-get --yes clean && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /hlint /hlint-scan /
ENTRYPOINT ["/hlint-scan"]
