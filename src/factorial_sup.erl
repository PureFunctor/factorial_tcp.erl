-module(factorial_sup).
-behaviour(supervisor).

-export([init/1, start_link/0]).
-export([start_socket/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(8080, [{packet, line}, {active, true}]),
    SupFlags =
	#{ strategy => simple_one_for_one
	 , intensity => 8
	 , period => 10
	 },
    ChildSpecs =
	[ #{ id => factorial_tcp
	   , start =>
		 { factorial_tcp
		 , start_link
		 , [ ListenSocket ]
		 }
	   , restart => temporary
	   , type => worker
	   , modules => [ factorial_tcp ]
	   }
	],
    spawn_link(fun empty_listeners/0),
    {ok, {SupFlags, ChildSpecs}}.

start_socket() ->
    supervisor:start_child(?MODULE, []).

empty_listeners() ->
    [ start_socket() || _ <- lists:seq(1, 20) ].
