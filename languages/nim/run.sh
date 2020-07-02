cat > program.nim
nim compile --run --colors=off --memTracker=off --verbosity=0 --hints=off --warnings=off --nimcache:/tmp/"$CODEDIR"/cache ./program.nim
