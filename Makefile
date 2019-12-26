# vim: noexpandtab filetype=make

.SUFFIXES: .erl .beam	

.erl.beam:
	erlc -W $<


ERL = erl -noshell -boot start_sasl


MODS = lb lb_sup proxy trait config 

all: compile
	${ERL} -config sys -s lb start
	
compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
