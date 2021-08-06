-module(factorial_tcp).
-behaviour(gen_server).

-export(
   [ init/1
   , handle_call/3
   , handle_cast/2
   , handle_info/2
   , start_link/1
   , terminate/2
   ]
).

-export([factorial/1, factorial/2]).

-record(state, {socket}).

%% Business Code

factorial(N) -> factorial(N, 1).

factorial(0, A) -> A;
factorial(N, A) -> factorial(N - 1, N * A).

%% Server Code

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    gen_server:cast(self(), accept),
    {ok, #state{socket=Socket}}.

handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(accept, State = #state{socket=ListenSocket}) ->
    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    gen_tcp:send(AcceptSocket, io_lib:format("Type a number or 'quit' to exit.~n", [])),
    factorial_sup:start_socket(),
    {noreply, State#state{socket=AcceptSocket}}.


handle_info({tcp, _Socket, "quit" ++ _}, State) ->
    gen_tcp:close(State#state.socket),
    {stop, normal, State};

handle_info({tcp, Socket, Message}, State) ->
    try list_to_integer(string:trim(Message)) of
	Number ->
	    if
		Number >= 100 ->
		    gen_tcp:send(Socket, io_lib:format("I wouldn't do that.~n", []));
		Number =< 0 ->
		    gen_tcp:send(Socket, io_lib:format("Positive numbers only.~n", []));
		true ->
		    gen_tcp:send(Socket, io_lib:format("Result: ~p~n", [factorial(Number)]))
	    end
    catch
	error:Error -> gen_tcp:send(Socket, io_lib:format("Enter a positive number.~n", []))
    end,
    {noreply, State};

handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};

handle_info({tcp_error, _Socket}, State) ->
    {stop, normal, State};

handle_info(Error, State) ->
    io:format("panic: ~p~n!", [Error]),
    {noreply, State}.


terminate(normal, _State) ->
    ok;

terminate(Reason, _State) ->
    io:format("terminated: ~p~n", [Reason]).
