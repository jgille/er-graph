%%% @author Jon Ivmark <jon@omega.avail.net>
%%% @copyright (C) 2012, Jon Ivmark
%%% @doc
%%%
%%% A graph module.
%%%
%%% @end
%%% Created : 19 Oct 2012 by Jon Ivmark <jon@omega.avail.net>

-module(graph).

%%% Client API
-export([start_link/0, stop/1]).
-export([get_neighbors/2, add_neighbor/3, rm_neighbor/3]).
-export([add/2, add/3, get/2, rm/2]).
-export([set_properties/3]).

%%% Server call backs
-export([call/3, run/2, init/0]).

-include("graph.hrl").

%%% Client API

start_link() -> graph_server:start_link(?MODULE).

stop(Pid) ->
    graph_server:call(Pid, stop).

get(Pid, Id) ->
    Node = get_pid(Pid, Id),
    get_node(Node).

add(Pid, Id) ->
    graph_server:call(Pid, {add, Id}).

add(Pid, Id, Properties) ->
    add(Pid, Id),
    set_properties(Pid, Id, Properties).

rm(Pid, Id) ->
    Node = get_pid(Pid, Id),
    rm(Pid, Node, Id).

set_properties(Pid, Id, Properties) ->
    Node = get_pid(Pid, Id),
    set_properties(Node, Properties).

get_neighbors(Pid, Id) ->
    Node = get_pid(Pid, Id),
    get_neighbors(Node).

add_neighbor(Pid, Start, End) ->
    mod_neighbors(Pid, Start, End, fun node:add_neighbor/2).

rm_neighbor(Pid, Start, End) ->
   mod_neighbors(Pid, Start, End, fun node:rm_neighbor/2).

%%% Server functions

call({get, Id}, Caller, Db) ->
    N = dict:find(Id, Db),
    graph_server:reply(Caller, N),
    Db;

call({add, Id}, Caller, Db) ->
    Node = mkNode(Id),
    % TODO: Check if key already in Db
    NewDb = dict:store(Id, Node, Db),
    graph_server:reply(Caller, node:get(Node)),
    NewDb;

call({rm, Id}, Caller, Db) ->
    NewDb = dict:erase(Id, Db),
    graph_server:reply(Caller, ok),
    NewDb;

call(stop, Caller, _Db) ->
    graph_server:reply(Caller, ok),
    io:format("Exiting..", []),
    exit(normal).

run({'EXIT', Node, Reason}, Db) ->
    case Reason of
        normal ->
            ok;
        _Other ->
            io:format("Caught unexpected exit signal. {Node: ~p, Reason: ~p}~n",
                      [Node, Reason]),
            % TODO: Optionally just remove the node instead of crashing
            erlang:error({"Node exited.", Node, Reason})
    end,
    Db;
run(_Msg, Db) ->
    % Default to doing nothing
    Db.

init() ->
    dict:new().

%%% Private helper functions

no_such_node() ->
    no_such_node. % TODO: Raise error?

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

rm(_Pid, no_such_node, _Id) ->
    no_such_node();
rm(Pid, Node, Id) ->
    node:stop(Node),
    graph_server:call(Pid, {rm, Id}).

set_properties(no_such_node, _Properties) ->
    no_such_node();
set_properties(Pid, Properties) ->
    node:set_properties(Pid, Properties).

mkNode(Id) ->
    Node = node:start_link(Id),
    Node.

get_neighbors(no_such_node) ->
    no_such_node();
get_neighbors(Pid) ->
    Neighbors = node:get_neighbors(Pid),
    Cur = cursor:from_list(Neighbors),
    cursor:map(fun node:get/1, Cur).

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
