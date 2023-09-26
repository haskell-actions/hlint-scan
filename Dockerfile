# syntax=docker/dockerfile:1

# We fetch and build a specific unreleased version of HLint as well,
# since a version of HLint with SARIF support has not been released yet.
#
# Once one has been, we may either continue to bundle the hlint binary
# together but at a more stable and official set of versions.
# Alternatively, we could have the action retrieve an hlint release
# automatically if one is not already available locally in the action.

FROM haskell:9.6.2-buster@sha256:31cc082049c1c14957289875e88ec69a386114e87cc7cacce35abcaff0f6a451 AS build
RUN git clone https://github.com/haskell-actions/hlint-scan.git /src/hlint-scan
WORKDIR /src/hlint-scan
RUN stack install hlint hlint-scan:exe:hlint-scan && \
    cp "$(stack path --local-bin)/hlint" "$(stack path --local-bin)/hlint-scan" /

FROM debian:buster-slim@sha256:70da746c9ef7c0fe4149ba1435c151f3239bab7a96b56b296e608794003cbf76
RUN apt-get --yes update && \
    apt-get --yes --no-install-recommends install ca-certificates=20200601~deb10u2 && \
    apt-get --yes clean && \
    rm -rf /var/lib/apt/lists/*
COPY --from=build /hlint /hlint-scan /
ENTRYPOINT ["/hlint-scan"]
