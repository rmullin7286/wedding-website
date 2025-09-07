#!/usr/bin/env sh

docker run \
    --name wedding-website \
    -p 8080:8080 \
    -v $HOME/.local/share/wedding:/data \
    -e WEDDING_PASSWORD=password \
    -e WEDDING_DATABASE=/data/wedding.db \
    wedding-website:latest
