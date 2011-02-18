.SUFFIXES: .erl .beam .yrl

.erl.beam:
	erlc -W $<

all: calc_server calc_client erl_compile

calc_server: calc.c cnode_server.c
	gcc -o cnode_server \
	-I/usr/lib/erlang/lib/erl_interface-3.7.1/include/ \
	-L/usr/lib/erlang/lib/erl_interface-3.7.1/lib/ \
	calc.c cnode_server.c \
	-lerl_interface -lei -lpthread -lnsl

calc_client: calc.c cnode_client.c
	gcc -o cnode_client \
	-I/usr/lib/erlang/lib/erl_interface-3.7.1/include/ \
	-L/usr/lib/erlang/lib/erl_interface-3.7.1/lib/ \
	calc.c cnode_client.c \
	-lerl_interface -lei -lpthread -lnsl

erl_compile:
	erl -compile calc

clean:
	rm -rf calc calc.o cnode_server cnode_server.o cnode_client cnode_client.o  *.beam
