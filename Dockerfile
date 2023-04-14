# syntax=docker/dockerfile:1

# We fetch and build a specific unreleased version of HLint as well,
# since a version of HLint with SARIF support has not been released yet.
#
# Once one has been, we may either continue to bundle the hlint binary
# together but at a more stable and official set of versions.
# Alternatively, we could have the action retrieve an hlint release
# automatically if one is not already available locally in the action.

FROM debian:stable AS build
RUN apt-get --yes update
RUN apt-get --yes install curl git gnupg ca-certificates
RUN curl -sSL https://get.haskellstack.org/ | sh
RUN mkdir -p /src
WORKDIR /src
RUN git clone https://github.com/haskell-actions/hlint-scan.git
WORKDIR /src/hlint-scan
RUN stack install hlint hlint-scan:exe:hlint-scan
RUN cp $(stack path --local-bin)/hlint /
RUN cp $(stack path --local-bin)/hlint-scan /

FROM debian:stable-slim
RUN apt-get --yes update
RUN apt-get --yes install ca-certificates
RUN apt-get --yes clean
COPY --from=build /hlint /hlint-scan /
ENTRYPOINT ["/hlint-scan"]
