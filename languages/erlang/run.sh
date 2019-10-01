echo "%% -*- erlang -*-" > program.erl
printf %s "$1" >> program.erl
escript program.erl || true
