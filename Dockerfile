FROM haskell:9.6.7 AS build

WORKDIR /app

COPY . .

RUN cabal update

RUN cabal install "exe:wedding-website" \
   --installdir=/bin \
   --install-method=copy \
   --overwrite-policy=always \
   -j

FROM debian:bookworm-slim AS runtime

WORKDIR /app

RUN apt-get update \
    && apt-get install -y --no-install-recommends libgmp10 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=build /bin/wedding-website ./wedding-website
COPY --from=build /app/static ./static

EXPOSE 8080

ENV WEDDING_DATABASE=/data/wedding.db

ENTRYPOINT ["./wedding-website"]
