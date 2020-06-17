cat > program.fs
fsharpc --optimize- program.fs >/dev/null && mono program.exe
