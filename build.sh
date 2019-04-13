#!/usr/bin/env sh

set -eo pipefail

rm -rf bootstrap
docker build . --tag lambdogs
docker rm dogs || true
docker create --name dogs lambdogs
docker cp dogs:/app/bootstrap bootstrap

cd $root_path
