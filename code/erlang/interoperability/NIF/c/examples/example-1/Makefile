.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

all: calc

calc: calc.c calc_nif.c
	gcc -o calc_nif.so -I/usr/lib/erlang/usr/include/ -fpic -shared calc.c calc_nif.c

clean:
	rm -rf calc calc_nif.so *.beam
