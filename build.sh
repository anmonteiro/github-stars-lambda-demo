#!/usr/bin/env sh

set -eo pipefail

rm -rf bootstrap
docker build . --tag github-stars
docker rm github-stars || true
docker create --name github-stars github-stars
docker cp github-stars:/app/bootstrap bootstrap

cd $root_path
