.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

#MODS = calc calc_lid

#all: ${MODS:%=%.beam} calc calc_lid

all: calc calc_drv.so

calc: calc.c erl_comm.c calc_driver.c
	gcc -o calc calc.c erl_comm.c calc_driver.c

calc_drv.so: calc.c #calc_lid.c
	gcc -o calc_drv.so -fpic -shared calc.c

clean:
	rm -rf calc calc_drv.so *.beam
