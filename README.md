# Wedding website for Ryan and Shaelyn Mullin

This is the website server code for Ryan and Shae's wedding website. All code here is licensed under AGPL.

## Setup

You'll need to install [GHCup](https://www.haskell.org/ghcup/) to install the core haskell tools to build this server.

Once it's installed, the tool versions used to build are:

| Tool  | Version  |
| ----- | -------- |
| GHC   | 9.6.7    |
| Cabal | 3.12.1.0 |
| HLS   | 2.10.0.0 |

## Tech Stack

* Server - `servant` and `servant-effectful`
* HTML Generation - `lucid`
* Effect system - `effectful`
* Database - SQLite with `sqlite-simple`
