FROM --platform=$BUILDPLATFORM haskell:9.8.4-slim AS build

WORKDIR /src

RUN apt-get update \
  && apt-get install -y --no-install-recommends ca-certificates zlib1g-dev \
  && rm -rf /var/lib/apt/lists/*

COPY cabal.project helheim.cabal ./
RUN cabal update && cabal build --only-dependencies all -j

COPY app ./app
COPY src ./src
COPY test ./test
COPY rinha-de-backend-2026/resources ./rinha-de-backend-2026/resources

RUN cabal build exe:helheim-api exe:helheim-build-index -j
RUN mkdir -p /out/bin /out/data \
  && cp "$(cabal list-bin exe:helheim-api)" /out/bin/helheim-api \
  && cp "$(cabal list-bin exe:helheim-build-index)" /out/bin/helheim-build-index

RUN /out/bin/helheim-build-index \
  --input rinha-de-backend-2026/resources/references.json.gz \
  --output /out/data/references.bin \
  --leaf-size 32

FROM debian:bookworm-slim AS runtime

ARG VCS_REF=unknown
ARG VERSION=dev
ARG CREATED=unknown

LABEL org.opencontainers.image.title="helheim" \
      org.opencontainers.image.description="Rinha de Backend 2026 fraud detector" \
      org.opencontainers.image.source="https://github.com/thales-maciel/helheim" \
      org.opencontainers.image.revision="${VCS_REF}" \
      org.opencontainers.image.version="${VERSION}" \
      org.opencontainers.image.created="${CREATED}" \
      org.opencontainers.image.licenses="MIT"

RUN apt-get update \
  && apt-get install -y --no-install-recommends ca-certificates libffi8 libgmp10 zlib1g \
  && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY --from=build /out/bin/helheim-api /app/helheim-api
COPY --from=build /out/data/references.bin /app/data/references.bin

ENV PORT=8080
ENV HELHEIM_INDEX=/app/data/references.bin

EXPOSE 8080

CMD ["/app/helheim-api", "+RTS", "-N1", "-A16m", "-I0", "-RTS", "--port", "8080", "--index", "/app/data/references.bin"]
