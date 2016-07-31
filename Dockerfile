FROM haskell
MAINTAINER /u/dmp1ce

# Install requirements for katy_playlist.hs
RUN cabal update && cabal install hoauth2 wreq
