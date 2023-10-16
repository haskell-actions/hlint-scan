# syntax=docker/dockerfile:1

# We fetch and build a specific unreleased version of HLint as well,
# since a version of HLint with SARIF support has not been released yet.
#
# Once one has been, we may either continue to bundle the hlint binary
# together but at a more stable and official set of versions.
# Alternatively, we could have the action retrieve an hlint release
# automatically if one is not already available locally in the action.

FROM haskell:9.6.3-buster@sha256:130fc3285fbbf4cded18e8404a90f5b462585b5b2e03ca7894d7be86b76f866b AS build
RUN git clone https://github.com/haskell-actions/hlint-scan.git /src/hlint-scan
WORKDIR /src/hlint-scan
RUN stack install hlint hlint-scan:exe:hlint-scan && \
    cp "$(stack path --local-bin)/hlint" "$(stack path --local-bin)/hlint-scan" /

FROM debian:buster-slim@sha256:bc2704bca194bb10ea0b52b4313dc44fc3339cc648457fb18cb3509e71f199b7
RUN apt-get --yes update && \
    apt-get --yes --no-install-recommends install ca-certificates=20200601~deb10u2 && \
    apt-get --yes clean && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /hlint /hlint-scan /
ENTRYPOINT ["/hlint-scan"]
