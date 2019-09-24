#!/usr/bin/env sh
set -o errexit -o xtrace

if ! docker network ls --filter name=itprotv_bridge | grep --quiet itprotv_bridge
then
  docker network create itprotv_bridge
fi

if ! test -f .env
then
  echo "PROJECT_DIR=$( pwd )" >> .env
fi

exec docker-compose up --build --force-recreate --remove-orphans

