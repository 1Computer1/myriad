printf %s "$1" > Main.idr
idris --execute ./Main.idr || true
