# vim: noexpandtab filetype=make

.SUFFIXES: .erl .beam	

.erl.beam:
	erlc -W $<


ERL = erl -noshell -boot start_sasl


MODS = proxy trait lb lb_sup

all: compile
	${ERL} -s lb start
	
compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
