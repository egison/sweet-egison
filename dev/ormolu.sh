#!/usr/bin/env bash

set -euo pipefail

readonly ORMOLU_VERSION="${ORMOLU_VERSION:-0.7.7.0}"

function main() {
  docker run -v $(pwd):/work --workdir /work ormolu/ormolu:${ORMOLU_VERSION} ormolu "$@"
}

main "$@"

