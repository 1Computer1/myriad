printf %s "$1" > program.jl
julia program.jl || true
