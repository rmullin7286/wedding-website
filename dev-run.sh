#!/bin/bash

# Hot reload development server for the wedding website
# Watches for changes in app/ directory and automatically rebuilds and restarts the server

export WEDDING_DATABASE="$HOME/.local/share/wedding/wedding.db"
export WEDDING_PASSWORD="password"

set -e

fd '\.hs$' | entr -r cabal run
