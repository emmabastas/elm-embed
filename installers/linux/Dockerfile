# Based initially on https://gist.github.com/rlefevre/1523f47e75310e28eee243c9c5651ac9
#
# Build Linux x64 binary from elm compiler top-level directory:
# $ docker build -t elm-embed -f installers/linux/Dockerfile .
#
# Retrieve elm Linux binary:
# $ docker cp $(docker create elm-embed):/usr/local/bin/elm-embed DESTINATION_DIRECTORY
#
# Delete docker elm-embed image:
# $ docker rmi elm-embed
#
# Display all images:
# $ docker images -a
#
# Delete all unused docker images:
# $ docker system prune -a

# Use Alpine 3.11 with GHC 8.6.5
FROM alpine:3.11

# Install packages required to build elm
RUN apk add --no-cache ghc cabal wget musl-dev zlib-dev zlib-static ncurses-dev ncurses-static

WORKDIR /elm-embed

# Import source code
COPY builder builder
COPY compiler compiler
COPY terminal terminal
COPY cabal.config elm-embed.cabal LICENSE ./

# Build statically linked elm binary
RUN cabal new-update
RUN cabal new-build --ghc-option=-optl=-static --ghc-option=-split-sections
RUN cp ./dist-newstyle/build/x86_64-linux/ghc-*/elm-embed-*/x/elm-embed/build/elm-embed/elm-embed /usr/local/bin/elm-embed

# Remove debug symbols to optimize the binary size
RUN strip -s /usr/local/bin/elm-embed
