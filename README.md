# Helheim

Haskell baseline for Rinha de Backend 2026.

The upstream contest repository is kept under `rinha-de-backend-2026/` as local
documentation, resources, and test scripts. This project implements the API
required by the contest:

- `GET /ready`
- `POST /fraud-score`

The current implementation is a correctness-first baseline. It vectorizes each
payload according to the contest rules and performs top-5 nearest-neighbor
search over a compact quantized reference index.

## Development

Install development tools with mise:

```sh
mise install
```

GHC is expected to be available on `PATH` through ghcup or your system package
manager. This machine currently uses GHC 9.4.8; the Docker build uses the
compiler bundled in the `haskell:9.8.4-slim` image.

Build and test:

```sh
mise run build
mise run test
```

Build the local compact index:

```sh
mise run build-index
```

Run one local API instance:

```sh
mise run api
```

Run the contest smoke test, with the API already listening on `localhost:9999`:

```sh
mise run smoke
```

## Docker

Run the local contest topology:

```sh
docker compose up --build
```

The compose file starts nginx on port `9999` and two API instances on the
bridge network. For the final `submission` branch, replace the local build with
a public `linux-amd64` image reference as required by the contest rules.

## Contest Notes

- The load balancer only proxies requests in round-robin; it does not inspect
  fraud payloads.
- The API returns `approved = fraud_score < 0.6`.
- `rinha-de-backend-2026/test/test-data.json` is never used as a lookup table.
- The repository includes an MIT license because the contest requires public
  submissions to be MIT licensed.
