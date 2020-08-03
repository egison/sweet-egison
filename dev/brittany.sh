#!/usr/bin/env bash

set -euo pipefail

readonly BRITTANY_VERSION="${BRITTANY_VERSION:-0.12.1.1}"

function main() {
  declare -a args_out
  for arg in "$@"; do
    if [[ "$arg" == *"./src/Control/Egison/Matcher/Collection.hs"* ]]; then
      continue;
    fi

    args_out+=( "$arg" )
  done

  docker run -v $(pwd):/work herpinc/brittany:${BRITTANY_VERSION} brittany "${args_out[@]}"
}

main "$@"
