/**
 * Example using a port drivers.
 * Copyright (c) 2011, Nycholas de Oliveira e Oliveira <nycholas@gmail.com>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the name of the Nycholas de Oliveira e Oliveira nor the names of
 *   its contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */
#include <stdlib.h>
#include <stdio.h>
#include "erl_driver.h"

typedef struct {
    ErlDrvPort port;
} calc_data;

static ErlDrvData calc_drv_start(ErlDrvPort port, char *buff) {
    calc_data* d = (calc_data*)driver_alloc(sizeof(calc_data));
    d->port = port;
    
    return (ErlDrvData)d;
}

static void calc_drv_stop(ErlDrvData handle) {
    driver_free((char*)handle);
}

static void calc_drv_output(ErlDrvData handle, char *buff, int bufflen) {
    calc_data* d = (calc_data*)handle;
    char fn, arg1, arg2, result;
    fn = buff[0];

    if (fn == 1) {
        arg1 = buff[1];
        arg2 = buff[2];

        //fprintf(stderr, "calling sum %i %i...\n", arg1, arg2);
        result = sum(arg1, arg2);
    } else if (fn == 2) {
        arg1 = buff[1];
        arg2 = buff[2];

        //fprintf(stderr, "calling subtract %i %i...\n", arg1, arg2);
        result = subtract(arg1, arg2);
    } else if (fn == 3) {
        arg1 = buff[1];
        arg2 = buff[2];

        //fprintf(stderr, "calling multiplies %i %i...\n", arg1, arg2);
        result = multiplies(arg1, arg2);
    } else if (fn == 4) {
        arg1 = buff[1];
        arg2 = buff[2];

        //fprintf(stderr, "calling divides %i %i...\n", arg1, arg2);
        result = divides(arg1, arg2);
    }
    driver_output(d->port, &result, 1);
}

ErlDrvEntry calc_driver_entry = {
    NULL,                /* F_PTR init, N/A */
    calc_drv_start,      /* L_PTR start, called when port is opened */
    calc_drv_stop,       /* F_PTR stop, called when port is closed */
    calc_drv_output,     /* F_PTR output, called when erlang has sent */
    NULL,                /* F_PTR ready_input, called when input descriptor ready */
    NULL,                /* F_PTR ready_output, called when output descriptor ready */
    "calc_drv",          /* char *driver_name, the argument to open_port */
    NULL,                /* F_PTR finish, called when unloaded */
    NULL,                /* F_PTR control, port_command callback */
    NULL,                /* F_PTR timeout, reserved */
    NULL                 /* F_PTR outputv, reserved */
};

DRIVER_INIT(calc_drv) { /* must match name in driver_entry */
    return &calc_driver_entry;
}
