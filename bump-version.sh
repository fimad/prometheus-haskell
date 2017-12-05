#!/bin/bash
# Bump the various hard coded version numbers.

set -e

if [ "$#" != "1" ]
then
  echo "usage: $0 VERSION"
  exit -1
fi

version="$1"

bump-changelog() {
  file="$1/CHANGELOG.md"
  echo "Updating $file"
  sed -i -r "s/^(## HEAD)$/\1\n\n## $version/" "$file"
}

bump-cabal() {
  cabal="$1"
  echo "Updating $cabal"

  # The version number of the package.
  sed -i -r "s/^(version:\s+)[0-9.]+$/\1$version/" "$cabal"
}

bump() {
  bump-changelog "$1"
  bump-cabal "$1/$1.cabal"
}


bump "prometheus-client"
bump "prometheus-metrics-ghc"
bump "wai-middleware-prometheus"

git diff --color=always | cat -
echo -en "\nDoes this look OK? [N/y] "
read ok && [ "$ok" == "y" ]

git commit -a -m "Bump version to $version"
