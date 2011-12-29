.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean

all: compile

compile:
	${ERL} -make

start: compile
	${ERL} -pa ebin

clean:
	rm -rf ./ebin/*.beam ./ebin/erl_crash.dump erl_crash.dump

test: compile
	${ERL} ./ebin/mycalc_test.erl
