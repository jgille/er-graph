%%% @author Jon Ivmark <jon@omega.avail.net>
%%% @copyright (C) 2012, Jon Ivmark
%%% @doc
%%%
%%% A graph module.
%%%
%%% @end
%%% Created : 19 Oct 2012 by Jon Ivmark <jon@omega.avail.net>

-module(graph).

-export([start/0, start_link/0, get/2, set_props/3, get_neighbors/2, add_neighbor/3, rm_neighbor/3, add/2, add/3, rm/2, quit/1]).
-export([call/3, run/2, init/0]).

-include("graph.hrl").

%%% Client API

start() -> graph_server:start(?MODULE).

start_link() -> graph_server:start_link(?MODULE).

no_such_node() ->
    no_such_node. % TODO: Raise error?

get(Pid, Id) ->
    Node = get_pid(Pid, Id),
    get_node(Node).

get_node(no_such_node) ->
    no_such_node();

get_node(Pid) ->
    node:get(Pid).

get_pid(Pid, Id) ->
    N = graph_server:call(Pid, {get, Id}),
    case N of
        {ok, NPid} -> NPid;
        error -> no_such_node
    end.

set_props(Pid, Id, Props) ->
    Node = get_pid(Pid, Id),
    set_props(Node, Props).

set_props(no_such_node, _Props) ->
    no_such_node();

set_props(Pid, Props) ->
    node:set_props(Pid, Props).

get_neighbors(Pid, Id) ->
    Node = get_pid(Pid, Id),
    get_neighbors(Node).

get_neighbors(no_such_node) ->
    no_such_node();

get_neighbors(Pid) ->
    Neighbors = node:get_neighbors(Pid),
    Cur = cursor:start(Neighbors),
    cursor:map(fun node:get/1, Cur).

add_neighbor(Pid, Start, End) ->
    mod_neighbors(Pid, Start, End, fun node:add_neighbor/2).

rm_neighbor(Pid, Start, End) ->
   mod_neighbors(Pid, Start, End, fun node:rm_neighbor/2).

mod_neighbors(Pid, Start, End, F) ->
    S = get_pid(Pid, Start),
    E = get_pid(Pid, End),
    mod_neighbors(S, E, F).

mod_neighbors(no_such_node, _End, _F) ->
    no_such_node();
mod_neighbors(_Start, no_such_node, _F) ->
    no_such_node();
mod_neighbors(S, E, F) ->
    F(S, E).

add(Pid, Id) ->
    graph_server:call(Pid, {add, Id}).

add(Pid, Id, Props) ->
    add(Pid, Id),
    set_props(Pid, Id, Props).

rm(Pid, Id) ->
    Node = get_pid(Pid, Id),
    rm(Pid, Node, Id).

rm(_Pid, no_such_node, _Id) ->
    no_such_node();

rm(Pid, Node, Id) ->
    node:quit(Node),
    graph_server:call(Pid, {rm, Id}).

quit(Pid) ->
    graph_server:call(Pid, quit).

%%% Server functions

call({get, Id}, Caller, Db) ->
    N = dict:find(Id, Db),
    graph_server:reply(Caller, N),
    Db;

call({add, Id}, Caller, Db) ->
    Node = mkNode(Id),
    % TODO: Check if key already in Db
    NewDb = dict:store(Id, Node, Db),
    graph_server:reply(Caller, Node),
    NewDb;

call({rm, Id}, Caller, Db) ->
    NewDb = dict:erase(Id, Db),
    graph_server:reply(Caller, ok),
    NewDb;

call(quit, Caller, _Db) ->
    graph_server:reply(Caller, ok),
    io:format("Exiting..", []),
    exit(normal).

run({'EXIT', Node, Reason}, Db) ->
    case Reason of
        normal ->
            ok;
        _Other ->
            io:format("Caught unexpected exit signal. {Node: ~p, Reason: ~p}~n", [Node, Reason]),
            % TODO: Optionally just remove the node instead of crashing
            erlang:error({"Node exited.", Node, Reason})
    end,
    Db;
run(_Msg, Db) ->
    % Default to doing nothing
    Db.

init() ->
    dict:new().

mkNode(Id) ->
    Node = node:start_link(Id),
    io:format("~p~n", [Node]),
    Node.
