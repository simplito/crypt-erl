-module(crypt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_, _) ->
    crypt_sup:start_link().

stop(_) -> 
    ok.