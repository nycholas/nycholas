.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<
    
ERL = erl -boot start_clean

MODS = mycalc

all: compile

start:
	${ERL} -pa '/home/nycholas/project/src/o_lalertom/nycholas/src/nycholas/code/erlang/OTP/examples/mycalc' -s mycalc start
	
test: compile
	./mycalc
    
compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
