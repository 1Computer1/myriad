printf %s "$1" > program.nim
nim compile --run --colors=off --memTracker=off --verbosity=0 ./program.nim || true
