# syntax=docker/dockerfile:1

# We fetch and build a specific unreleased version of HLint as well,
# since a version of HLint with SARIF support has not been released yet.
#
# Once one has been, we may either continue to bundle the hlint binary
# together but at a more stable and official set of versions.
# Alternatively, we could have the action retrieve an hlint release
# automatically if one is not already available locally in the action.

FROM haskell:9.4.5-buster@sha256:81f675c4711ed066969bfd92ad9ed2e8827ce8b8858ff6dc1aaf0d1c66e3d97f AS build
RUN git clone https://github.com/haskell-actions/hlint-scan.git /src/hlint-scan
WORKDIR /src/hlint-scan
RUN stack install hlint hlint-scan:exe:hlint-scan && \
    cp "$(stack path --local-bin)/hlint" "$(stack path --local-bin)/hlint-scan" /

FROM debian:buster-slim@sha256:fdb38c743a538d301ddcedd3047c43bf4fcc70211c8534c5b613916910fe1b9d
RUN apt-get --yes update && \
    apt-get --yes --no-install-recommends install ca-certificates=20200601~deb10u2 && \
    apt-get --yes clean && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /hlint /hlint-scan /
ENTRYPOINT ["/hlint-scan"]
