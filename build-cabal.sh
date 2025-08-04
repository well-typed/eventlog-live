#!/usr/bin/env bash

set -e
out=_cabal_out
dir=/home/matt/ghc-wen/libraries/Cabal/Cabal/src
if [[ -z "$GHC" ]]; then
  GHC="_build/stage1/bin/ghc"
fi
$GHC -fno-code -fforce-recomp -j -hidir $out -odir $out -i$dir Distribution.Simple "$@"
rm -R $out
