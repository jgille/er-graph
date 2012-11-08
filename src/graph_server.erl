%%% @author Jon Ivmark <jon@omega.avail.net>
%%% @copyright (C) 2012, Jon Ivmark
%%% @doc
%%%
%%% A generic server module that should probably be removed in favour of some otp counter part.
%%%
%%% @end
%%% Created : 19 Oct 2012 by Jon Ivmark <jon@omega.avail.net>

-module(graph_server).
-export([start_link/1, start_link/2, call/2, run/2, reply/2]).

-define(TIMEOUT, 5000).

%%% Public API

start_link(Module) ->
    spawn_link(fun() -> init(Module) end).

start_link(Module, InitialVal) ->
    spawn_link(fun() -> init(Module, InitialVal) end).

call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {call, self(), Ref, Msg},
    receive
        {Ref, Reply} ->
            erlang:demonitor(Ref, [flush]),
            Reply;
        {'DOWN', Ref, process, Pid, Reason} ->
            erlang:error(Reason)
    after ?TIMEOUT ->
            % Crash on timeout, really?
            erlang:error(timeout)
    end.

run(Pid, Msg) ->
    Pid ! {run, Msg},
    ok.

reply({Pid, Ref}, Reply) ->
    Pid ! {Ref, Reply}.

%%% Private stuff
init(Module) ->
    process_flag(trap_exit, true),
    loop(Module, Module:init()).

init(Module, InitialVal) ->
    process_flag(trap_exit, true),
    loop(Module, Module:init(InitialVal)).

loop(Module, State) ->
    receive
        {run, Msg} ->
            loop(Module, Module:run(Msg, State));
        {call, Pid, Ref, Msg} ->
            loop(Module, Module:call(Msg, {Pid, Ref}, State));
        Other ->
            loop(Module, Module:run(Other, State))
    end.
