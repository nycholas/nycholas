.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<
    
ERL = erl -boot start_clean

MODS = socket_server server client mycalc

all: compile

start: clean compile
	${ERL} -pa '/home/nycholas/project/src/o_lalertom/nycholas/src/nycholas/code/erlang/OTP/examples/mycalc-tcp' -s server start
	
test: compile
	./mycalc
    
compile: ${MODS:%=%.beam}

clean:
	rm -rf *.beam erl_crash.dump
