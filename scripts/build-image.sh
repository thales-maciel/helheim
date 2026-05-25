#!/usr/bin/env sh
set -eu

image="${HELHEIM_IMAGE:-ghcr.io/thales-maciel/helheim}"
commit="${HELHEIM_COMMIT:-$(git rev-parse --short=12 HEAD)}"
created="${HELHEIM_CREATED:-$(date -u +%Y-%m-%dT%H:%M:%SZ)}"
tracked_dirty="$(git status --porcelain --untracked-files=no)"

if [ -n "$tracked_dirty" ] && [ "${HELHEIM_ALLOW_DIRTY:-0}" != "1" ]; then
  printf '%s\n' "tracked files are dirty; commit first or set HELHEIM_ALLOW_DIRTY=1" >&2
  exit 1
fi

if [ -n "${HELHEIM_TAG:-}" ]; then
  tag="$HELHEIM_TAG"
elif [ -n "$tracked_dirty" ]; then
  tag="$commit-dirty"
else
  tag="$commit"
fi

docker buildx build \
  --platform linux/amd64 \
  --build-arg "VCS_REF=$commit" \
  --build-arg "VERSION=$tag" \
  --build-arg "CREATED=$created" \
  -t "$image:$tag" \
  "$@" \
  .

printf '%s\n' "$image:$tag"
