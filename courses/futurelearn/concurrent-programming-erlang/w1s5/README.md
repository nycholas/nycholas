# Server Examples

### Compile

```
$ erl
Erlang/OTP 22 [erts-10.6.4] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1]

Eshell V10.6.4  (abort with ^G)
1> [c(M) || M <- [server, client, palin]].
[{ok,server},{ok,client},{ok,palin}]
```

### Server V1

```
9> Self = self().
<0.78.0>
10> Server = spawn(server, server, [Self]).
<0.135.0>
13> Server ! {check, "Madam I\'m Adam"}.
{check,"Madam I'm Adam"}
14> flush().                            
Shell got {result,[34,"Madam I'm Adam",34,32,105,115,32,97,32,112,97,108,105,
                   110,100,114,111,109,101]}
ok
```

### Server V2

```
18> Self = self().
<0.78.0>
21> Server = spawn(server, server_v2, []).
<0.151.0>
22> Server ! {Self, {check, "A man, a plan, a canal, Panama!"}}.
{<0.78.0>,{check,"A man, a plan, a canal, Panama!"}}
23> flush().
Shell got {result,[34,"A man, a plan, a canal, Panama!",34,32,105,115,32,110,
                   111,116,32,97,32,112,97,108,105,110,100,114,111,109,101]}
ok
```

### Client

```
3> Self = self().
<0.78.0>
4> Server = spawn(server, server, []).
<0.98.0>
5> Client = spawn(client, start, [Server]).
<0.100.0>
6> Client ! {Self, {check, "HA"}}.
{<0.78.0>,{check,"HA"}}
7> flush().
Shell got {result,[34,"HA",34,32,105,115,32,110,111,116,32,97,32,112,97,108,
                   105,110,100,114,111,109,101]}
ok
```

### Balancer

```
31> self().
<0.128.0>
34> S1 = spawn(server, server, []).                
<0.145.0>
35> S2 = spawn(server, server, []).        
<0.147.0>
36> S3 = spawn(server, server, []).
<0.149.0>
37> Balancer = spawn(server, balancer, [[S1, S2, S3]]).
<0.151.0>
38> Balancer ! {self(), {check, "Madam I\'m Adam"}}.     
Received message: balancer(<0.151.0>) executing by server(<0.145.0>)
{<0.128.0>,{check,"Madam I'm Adam"}}
Received message: from(<0.128.0>) on server(<0.145.0>)
40> Balancer ! {self(), {check, "Madam I\'m Adam"}}.
Received message: balancer(<0.151.0>) executing by server(<0.147.0>)
Received message: from(<0.128.0>) on server(<0.147.0>)
{<0.128.0>,{check,"Madam I'm Adam"}}
41> Balancer ! {self(), {check, "Madam I\'m Adam"}}.
Received message: balancer(<0.151.0>) executing by server(<0.149.0>)
{<0.128.0>,{check,"Madam I'm Adam"}}
Received message: from(<0.128.0>) on server(<0.149.0>)
42> Balancer ! {self(), {check, "Madam I\'m Adam"}}.
Received message: balancer(<0.151.0>) executing by server(<0.145.0>)
{<0.128.0>,{check,"Madam I'm Adam"}}
Received message: from(<0.128.0>) on server(<0.145.0>)
43> flush().
Shell got {result,[34,"Madam I'm Adam",34,32,105,115,32,97,32,112,97,108,105,
                   110,100,114,111,109,101]}
Shell got {result,[34,"Madam I'm Adam",34,32,105,115,32,97,32,112,97,108,105,
                   110,100,114,111,109,101]}
Shell got {result,[34,"Madam I'm Adam",34,32,105,115,32,97,32,112,97,108,105,
                   110,100,114,111,109,101]}
Shell got {result,[34,"Madam I'm Adam",34,32,105,115,32,97,32,112,97,108,105,
                   110,100,114,111,109,101]}
ok

```