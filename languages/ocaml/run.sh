cat > program.ml
ocamlopt -cclib --static -o program program.ml && ./program
