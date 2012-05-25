.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean
ERL_PROJECT_DIR = ebin priv include

all: compile

compile:
	mkdir -p ${ERL_PROJECT_DIR}
	${ERL} -make

start: compile
	${ERL} -pa ebin

clean:
	rm -rf ./ebin/*.beam ./ebin/erl_crash.dump erl_crash.dump

test: compile
	${ERL} ./ebin/mycalc_test.erl
