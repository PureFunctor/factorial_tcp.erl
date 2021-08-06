# factorial_tcp.erl
A simple OTP application running a TCP server with supervised gen_server handlers.

```sh
$ rebar3 shell
===> Verifying dependencies...
===> Analyzing applications...
===> Compiling factorial
Erlang/OTP 24 [erts-12.0.3] [source] [64-bit] [smp:2:2] [ds:2:2:10] [async-threads:1] [jit]

Eshell V12.0.3  (abort with ^G)
1> ===> Booted factorial

...

$ telnet localhost 8080
Trying ::1...
Connection failed: Connection refused
Trying 127.0.0.1...
Connected to localhost.
Escape character is '^]'.
Type a number or 'quit' to exit.
5
Result: 120
100
I wouldn't do that.
quit
Connection closed by foreign host.
```
