printf %s "$1" > program.cs && csc program.cs | awk 'NR>2'
mono program.exe || true
