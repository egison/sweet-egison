#!/usr/bin/env bash

set -euo pipefail

readonly HOOK_FILE=$(git rev-parse --git-dir)/hooks/pre-commit

function needs_install() {
  local signature="$1"

  if [ -f $HOOK_FILE ]; then
    if grep -q "$signature" $HOOK_FILE; then
      # file found, signature found
      return 1
    else
      # file found, signature not found
      return 0
    fi
  else
    # file not found
    return 0
  fi
}

function main() {
  local signature="DO NOT EDIT: Installed by ./dev/install_pre_commit_hook.sh"

  local source=$(cat << EOF
#!/usr/bin/env bash

# $signature

if ! git ls-files '*.hs' | xargs ./dev/brittany.sh --check-mode; then
  >&2 echo "fatal: Some files need formatting. Aborted."
  exit 1
fi

if ! git ls-files '*.hs' | xargs hlint; then
  >&2 echo "fatal: Some files need linting. Aborted."
  exit 1
fi

shopt -s globstar nullglob
if ! diff <(cabal-fmt **/*.cabal) <(cat **/*.cabal); then
  >&2 echo "fatal: cabal file needs formatting. Aborted."
  exit 1
fi

# END $signature
EOF
)

  if needs_install "$signature"; then
    echo -n "$source" >> $HOOK_FILE
    chmod +x $HOOK_FILE
  else
    >&2 echo "fatal: No need to install. Aborted."
    exit 1
  fi
}

main
