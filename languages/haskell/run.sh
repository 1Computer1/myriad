printf %s "$1" > program.hs
ghc -e main program.hs || true
