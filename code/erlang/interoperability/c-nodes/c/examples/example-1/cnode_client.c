#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

int main(int argc, char **argv) {
    int fd;                                          /* fd to Erlang node */
    int loop = 1;                                    /* Loop flag */
    int got;                                         /* Result of receive */
    unsigned char buf[BUFSIZE];                      /* Buffer for incoming message */
    ErlMessage emsg;                                 /* Incoming message */
    ETERM *fromp, *tuplep, *fnp, *arg1, *arg2, *resp;
    int res;

    char hostname[] = "chatterer";
    char nodename[] = "cenobites";
    char fullnodename[] = "cenobites@cenobites.org";
    char cookie[] = "secretcookie";

    fprintf(stdout, "Starting the memory manager...\n\r");
    erl_init(NULL, 0);

    fprintf(stdout, "Setting cookie '%s'...\n\r", cookie);
    if (erl_connect_init(1, cookie, 0) == -1) {
        erl_err_quit("erl_connect_init");
    }

    fprintf(stdout, "Connecting in %s...\n\r", fullnodename);
    if ((fd = erl_connect(fullnodename)) < 0) {
        erl_err_quit("erl_connect");
    }
    
    fprintf(stdout, "Connected to %s\n\r", fullnodename);
    while (loop) {
        got = erl_receive_msg(fd, buf, BUFSIZE, &emsg);
        if (got == ERL_TICK) {
            /* ignore */
        } else if (got == ERL_ERROR) {
            loop = 0;
        } else {
            if (emsg.type == ERL_REG_SEND) {
                fromp = erl_element(2, emsg.msg);
                tuplep = erl_element(3, emsg.msg);
                fnp = erl_element(1, tuplep);
                arg2 = erl_element(2, tuplep);
                arg1 = erl_element(3, tuplep);
                
                if (strncmp(ERL_ATOM_PTR(fnp), "sum", 3) == 0) {
                    fprintf(stdout, "Calling sum...\n\r");
                    res = sum(ERL_INT_VALUE(arg1), ERL_INT_VALUE(arg2));
                } else if (strncmp(ERL_ATOM_PTR(fnp), "subtract", 8) == 0) {
                    fprintf(stdout, "Calling subtract...\n\r");
                    res = subtract(ERL_INT_VALUE(arg1), ERL_INT_VALUE(arg2));
                } else if (strncmp(ERL_ATOM_PTR(fnp), "multiplies", 10) == 0) {
                    fprintf(stdout, "Calling multiplies...\n\r");
                    res = multiplies(ERL_INT_VALUE(arg1), ERL_INT_VALUE(arg2));
                } else if (strncmp(ERL_ATOM_PTR(fnp), "divides", 7) == 0) {
                    fprintf(stdout, "Calling divides...\n\r");
                    res = divides(ERL_INT_VALUE(arg1), ERL_INT_VALUE(arg2));
                }
                
                resp = erl_format("{cenobites, ~i}", res);
                erl_send(fd, fromp, resp);
                
                erl_free_term(emsg.from);
                erl_free_term(emsg.msg);
                erl_free_term(fromp);
                erl_free_term(tuplep);
                erl_free_term(fnp);
                erl_free_term(arg1);
                erl_free_term(arg2);
                erl_free_term(resp);
            }
        }
    }
}
