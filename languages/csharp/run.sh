printf %s "$1" > program.cs
csc program.cs && mono program.exe || true
