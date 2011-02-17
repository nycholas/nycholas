.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

all: calc

calc: calc.c calc_driver.c
	gcc -o calc_drv.so -I/usr/lib/erlang/usr/include/ -fpic -shared calc.c calc_driver.c

clean:
	rm -rf calc calc_drv.so *.beam
