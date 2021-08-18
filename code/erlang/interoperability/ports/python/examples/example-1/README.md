# example-1

## RUNNING

```
$ erl
Erlang R14B (erts-5.8.1) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.8.1  (abort with ^G)
1> c(calc).
{ok,calc}
2> calc:start().
<0.39.0>
3> calc:sum(3, 4).
7
4> calc:subtract(4, 1).
3
5> calc:multiplies(4, 4).
16
6> calc:divides(4, 2).   
2
```

## SERVER/CLIENT RUNNING

```
$ erl -name chattarer
Erlang R14B (erts-5.8.1) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.8.1  (abort with ^G)
(chattarer@localhost.localdomain)1> c(calc).
{ok,calc}
(chattarer@localhost.localdomain)2> calc:start().
<0.45.0>

$ erl -name pinhead
Erlang R14B (erts-5.8.1) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.8.1  (abort with ^G)
(pinhead@localhost.localdomain)1> rpc:call(chattarer@localhost.localdomain, calc, sum, [2, 3]).
5
``` 