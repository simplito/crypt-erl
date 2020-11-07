-module(crypt_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Child = #{ 
        id => crypt, 
        start => {crypt, start_link, []}
    },    
    RestartStrategy = #{ 
        strategy  => one_for_one, 
        intensity => 5,
        period => 1
    },
    {ok, {RestartStrategy, [Child]}}.