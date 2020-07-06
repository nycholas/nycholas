# Exercise: Upgrading the frequency server

```
$ erl
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]

Eshell V10.6.4  (abort with ^G)
1> c(frequency).
frequency.erl:102: Warning: function handle_inject/2 is unused
{ok,frequency}
2> frequency:start().
{ok,<0.97.0>}
3> frequency:allocate().
{ok,10}
5> frequency:inject([1]).
{error,timeouted}
6> c(frequency).
{ok,frequency}
7> frequency:inject([1]).                      
{error,timeouted}
8> code:soft_purge(frequency).
true
9> frequency:inject([1]).     
ok
```