export GOCACHE=/tmp/"$CODEDIR"/cache
cat > program.go
go run program.go || true
