# vim: noexpandtab filetype=make

.SUFFIXES: .erl .beam	

.erl.beam:
	erlc -W $<


ERL = erl -noshell -boot start_clean


MODS = proxy trait

all: compile
	${ERL} -s proxy main
	
compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
