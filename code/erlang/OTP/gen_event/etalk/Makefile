.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

.yrl.erl:
	erlc -W $<

ERL = erl -boot start_clean
ERL_PROJECT_DIR = ebin include priv release test

all: compile

compile:
	mkdir -p ${ERL_PROJECT_DIR}
	${ERL} -make

run-shell: compile
	${ERL} -pa ebin

start: compile
	${ERL} -sname etalk_dev -pa ebin -eval 'application:start(etalk)'

clean:
	rm -rf ./ebin/*.beam ./ebin/erl_crash.dump erl_crash.dump

test: compile
	${ERL} ./test/etalk_tests.erl
