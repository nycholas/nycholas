/**
 * Example using NIF.
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
#include "erl_nif.h"

extern int sum(int a, int b);
extern int subtract(int a, int b);
extern int multiplies(int a, int b);
extern int divides(int a, int b);

static ERL_NIF_TERM sum_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int a, b, ret;
    
    if (!enif_get_int(env, argv[0], &a) || !enif_get_int(env, argv[1], &b)) {
        return enif_make_badarg(env);
    }
    ret = sum(a, b);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM subtract_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int a, b, ret;
    
    if (!enif_get_int(env, argv[0], &a) || !enif_get_int(env, argv[1], &b)) {
        return enif_make_badarg(env);
    }
    ret = subtract(a, b);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM multiplies_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int a, b, ret;

    if (!enif_get_int(env, argv[0], &a) || !enif_get_int(env, argv[1], &b)) {
        return enif_make_badarg(env);
    }
    ret = multiplies(a, b);
    return enif_make_int(env, ret);
}

static ERL_NIF_TERM divides_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    int a, b, ret;

    if (!enif_get_int(env, argv[0], &a) || !enif_get_int(env, argv[1], &b)) {
        return enif_make_badarg(env);
    }
    ret = divides(a, b);
    return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"sum", 2, sum_nif},
    {"subtract", 2, subtract_nif},
    {"multiplies", 2, multiplies_nif},
    {"divides", 2, divides_nif}
};

ERL_NIF_INIT(calc, nif_funcs, NULL, NULL, NULL, NULL)
