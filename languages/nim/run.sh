cat > program.nim
nim compile --run --colors=off --memTracker=off --verbosity=0 --hints=off --nimcache:/tmp/"$CODEDIR"/cache ./program.nim || true
