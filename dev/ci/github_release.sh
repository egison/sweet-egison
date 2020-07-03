#!/usr/bin/env bash

set -euo pipefail
shopt -s globstar

function main() {
  local -r repo_url="https://github.com/${GITHUB_REPOSITORY}"
  local -r version=${GITHUB_REF#refs\/tags\/v}

  local -r release_badge="[![release](${repo_url}/workflows/${GITHUB_WORKFLOW}/badge.svg)](${repo_url}/actions/runs/${GITHUB_RUN_ID})"
  local release_message
  release_message="$(git tag -l --format='%(contents)' v${version})"

  local package_messages=''

  for cabal_file in **/*.cabal; do
    local package package_dir
    package="$(sed -e 's/^name:\s*\(.*\)$/\1/;t;d' $cabal_file)"
    package_dir="$(dirname $cabal_file)"

    local package_badge="[![Hackage](https://img.shields.io/badge/hackage-v${version}-blue)](https://hackage.haskell.org/package/${package}-${version})"
    local changelog_content=$(< ./${package_dir}/CHANGELOG.md)
    local front_trimmed="${changelog_content#*\#\# ${version}}"
    local package_message="${front_trimmed%%\#\#*}"

    package_messages=$(cat << EOS
${package_messages}

## ${package} ${package_badge}

${package_message}
EOS
)
  done

  local body=$(cat << EOS
${release_badge}
${release_message}
${package_messages}
EOS
)

  # https://github.community/t5/GitHub-Actions/set-output-Truncates-Multiline-Strings/td-p/37870
  body="${body//'%'/'%25'}"
  body="${body//$'\n'/'%0A'}"
  body="${body//$'\r'/'%0D'}"

  echo "::set-output name=body::${body}"
  echo "::set-output name=name::v${version}"
}

main
