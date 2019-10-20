#!/bin/sh
set -e

printf %s "$1" | ts-node -p || true