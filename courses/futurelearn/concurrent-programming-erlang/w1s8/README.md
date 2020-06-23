# Exercise: Working with the mailbox

In order for the messages to have arrived in the mailbox of the receiver before they are processed, you can use a call to timer:sleep(M), which will sleep a process for M milliseconds, before performing the receive statement. (timer:sleep(500))

```
40> Server = spawn(mailbox, start, []).
<0.277.0>
41> ok = mailbox:send(Server, "Olá mundo!").
message: [79,108,225,32,109,117,110,100,111,33]
ok
52> [mailbox:send(Server, N) || N <- lists:seq(0, 100)].
message: 0
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,
 ok,ok,ok,ok,ok,ok,ok,ok,ok,ok|...]
message: 1
message: 2
message: 3
message: 4
message: 5
message: 6
message: 7
message: 8
message: 9
message: 10
message: 11
message: 12
message: 13
message: 14
message: 15
message: 16
message: 17
message: 18
message: 19
message: 20
message: 21
message: 22
message: 23
message: 24
message: 25
message: 26
message: 27
message: 28
message: 29
message: 30
message: 31
message: 32
message: 33
message: 34
message: 35
message: 36
message: 37
message: 38
message: 39
message: 40
message: 41
message: 42
message: 43
message: 44
message: 45
message: 46
message: 47
message: 48
message: 49
message: 50
message: 51
message: 52
message: 53
message: 54
message: 55
message: 56
message: 57
message: 58
message: 59
message: 60
message: 61
message: 62
message: 63
message: 64
message: 65
message: 66
message: 67
message: 68
message: 69
message: 70
message: 71
message: 72
message: 73
message: 74
message: 75
message: 76
message: 77
message: 78
message: 79
message: 80
message: 81
message: 82
message: 83
message: 84
message: 85
message: 86
message: 87
message: 88
message: 89
message: 90
message: 91
message: 92
message: 93
message: 94
message: 95
message: 96
message: 97
message: 98
message: 99
message: 100
```

If you remove the call to timer:sleep/1, would you expect the behaviour of the receiver to change? Does it change when you try it out? (without timer:sleep(500))

```
56> Server = spawn(mailbox, start, []).
<0.326.0>
57> [mailbox:send(Server, N) || N <- lists:seq(0, 100)].
message: 0
message: 1
[ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,ok,
 ok,ok,ok,ok,ok,ok,ok,ok,ok,ok|...]
message: 2
message: 3
message: 4
message: 5
message: 6
message: 7
message: 8
message: 9
message: 10
message: 11
message: 12
message: 13
message: 14
message: 15
message: 16
message: 17
message: 18
message: 19
message: 20
message: 21
message: 22
message: 23
message: 24
message: 25
message: 26
message: 27
message: 28
message: 29
message: 30
message: 31
message: 32
message: 33
message: 34
message: 35
message: 36
message: 37
message: 38
message: 39
message: 40
message: 41
message: 42
message: 43
message: 44
message: 45
message: 46
message: 47
message: 48
message: 49
message: 50
message: 51
message: 52
message: 53
message: 54
message: 55
message: 56
message: 57
message: 58
message: 59
message: 60
message: 61
message: 62
message: 63
message: 64
message: 65
message: 66
message: 67
message: 68
message: 69
message: 70
message: 71
message: 72
message: 73
message: 74
message: 75
message: 76
message: 77
message: 78
message: 79
message: 80
message: 81
message: 82
message: 83
message: 84
message: 85
message: 86
message: 87
message: 88
message: 89
message: 90
message: 91
message: 92
message: 93
message: 94
message: 95
message: 96
message: 97
message: 98
message: 99
message: 100
```