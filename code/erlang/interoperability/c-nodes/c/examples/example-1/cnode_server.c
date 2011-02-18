#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include "erl_interface.h"
#include "ei.h"

#define BUFSIZE 1000

int main(int argc, char **argv) {
    struct in_addr addr;          /* 32-bit IP number of host */
    int listen;                   /* Listen socket */
    int fd;                       /* fd to Erlang node */
    ErlConnect conn;              /* Connection data */
    int loop = 1;                 /* Loop flag */
    int got;                      /* Result of receive */
    unsigned char buf[BUFSIZE];   /* Buffer for incoming message */
    ErlMessage emsg;              /* Incoming message */
    ETERM *fromp, *tuplep, *fnp, *arg1, *arg2, *resp;
    int res;

    char hostname[] = "pihhead";
    char nodename[] = "cenobites";
    char fullnodename[] = "pinhead@cenobites.org";
    char domainname[] = "cenobites.org";
    char addrname[] = "10.10.100.5";
    char cookie[] = "secretcookie";
    int port;

    port = atoi(argv[1]);
    
    fprintf(stdout, "Starting the memory manager...\n\r");
    erl_init(NULL, 0);
    addr.s_addr = inet_addr(addrname);

    fprintf(stdout, "Starting the node '%s'...\n\r", fullnodename);
    if (erl_connect_xinit(hostname, nodename, fullnodename,
                          &addr, cookie, 0) == -1) {
        erl_err_quit("erl_connect_xinit");
    }

    /* Make a listen socket */
    fprintf(stdout, "Make a listen socket...\n\r");
    if ((listen = calc_listen(port)) <= 0) {
        erl_err_quit("calc_listen");
    }

    fprintf(stdout, "Publishing on the door %d...\n\r", port);
    if (erl_publish(port) == -1) {
        erl_err_quit("erl_publish");
    }

    fprintf(stdout, "Accepting connections we...\n\r");
    if ((fd = erl_accept(listen, &conn)) == ERL_ERROR) {
        erl_err_quit("erl_accept");
    }
    
    fprintf(stdout, "Connected to %s\n\r", conn.nodename);
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
                arg1 = erl_element(2, tuplep);
                arg2 = erl_element(3, tuplep);
                
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

    return 0;
}

int calc_listen(int port) {
    int listen_fd;
    struct sockaddr_in addr;
    int on = 1;

    fprintf(stdout, "Open socket...\n\r");
    if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        return -1;
    }

    fprintf(stdout, "Setting config socket...\n\r");
    setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on));
    
    memset((void*) &addr, 0, (size_t) sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    fprintf(stdout, "Binding socket...\n\r");
    if (bind(listen_fd, (struct sockaddr*) &addr, sizeof(addr)) < 0) {
        return -1;
    }

    fprintf(stdout, "Listen socket...\n\r");
    listen(listen_fd, 5);
    
    return listen_fd;
}
