# Exercise: Scaling the frequency server

```
9> frequency:start_link([{poll_servers, 3}]).                                                  
{ok,<0.119.0>}
10> frequency:info().                                           
[{'frequency-1',[10,11,12,13,14,25],<0.120.0>},
 {'frequency-2',[15,16,17,18,19],<0.121.0>},
 {'frequency-3',[20,21,22,23,24],<0.122.0>}]
11> {ok, Freq1} = frequency:allocate().                         
{ok,10}
12> frequency:info().                         
[{'frequency-2',[15,16,17,18,19],<0.121.0>},
 {'frequency-3',[20,21,22,23,24],<0.122.0>},
 {'frequency-1',[10,11,12,13,14,25],<0.120.0>}]
13> ok = frequency:deallocate(Freq1).                           
ok
14> frequency:info().
[{'frequency-2',[15,16,17,18,19],<0.121.0>},
 {'frequency-3',[20,21,22,23,24],<0.122.0>},
 {'frequency-1',[10,11,12,13,14,25],<0.120.0>}]
15> exit(S1).
** exception exit: <0.121.0>
16> {ok, Freq2} = frequency:allocate().
{ok,15}
17> ok = frequency:deallocate(Freq2).
ok
18> ok = frequency:inject([1, 2, 3, 4, 5, 6]).
ok
19> frequency:info().
[{'frequency-3',[20,21,22,23,24,1,2],<0.122.0>},
 {'frequency-1',[10,11,12,13,14,25,3,4],<0.120.0>},
 {'frequency-2',[15,16,17,18,19,5,6],<0.121.0>}]
```