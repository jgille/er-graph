%%% @author Jon Ivmark <jon@omega.avail.net>
%%% @copyright (C) 2012, Jon Ivmark
%%% @doc
%%%
%%% A module that handles individual nodes in a graph.
%%%
%%% @end
%%% Created : 19 Oct 2012 by Jon Ivmark <jon@omega.avail.net>

-module(node).

-export([start/1, start_link/1, get/1, set_props/2, get_neighbors/1, add_neighbor/2, rm_neighbor/2, find_property/2, quit/1]).
-export([call/3, run/2, init/1]).

-include("graph.hrl").

%%% Client API

start(Id) -> graph_server:start(?MODULE, Id).

start_link(Id) -> graph_server:start_link(?MODULE, Id).

get(Pid) ->
    graph_server:call(Pid, get).

set_props(Pid, Props) ->
    graph_server:call(Pid, {set_props, orddict:from_list(Props)}).

get_neighbors(Pid) ->
    graph_server:call(Pid, get_neighbors).

add_neighbor(Pid, Neighbor) ->
    graph_server:call(Pid, {add_neighbor, Neighbor}).

rm_neighbor(Pid, Neighbor) ->
    graph_server:call(Pid, {rm_neighbor, Neighbor}).

find_property(Node, Key) ->
    Props = Node#node.props,
    orddict:find(Key, Props).

quit(Pid) ->
    graph_server:call(Pid, quit).

%%% Server functions

call(get, Caller, Node) ->
    graph_server:reply(Caller, as_node(Node)),
    Node;

call({set_props, Props}, Caller, Node) ->
    NewNode = Node#graph_node{props=Props},
    graph_server:reply(Caller, as_node(NewNode)),
    NewNode;

call(get_neighbors, Caller, Node) ->
    graph_server:reply(Caller, Node#graph_node.neighbors),
    Node;

call({add_neighbor, Neighbor}, Caller, Node) ->
    erlang:monitor(process, Neighbor),
    NewNode = Node#graph_node{neighbors=[Neighbor|Node#graph_node.neighbors]},
    graph_server:reply(Caller, as_node(NewNode)),
    NewNode;

call({rm_neighbor, Neighbor}, Caller, Node) ->
    NewNode = rm_node_neighbor(Node, Neighbor),
    graph_server:reply(Caller, as_node(NewNode)),
    NewNode;

call(quit, Caller, _) ->
    graph_server:reply(Caller, ok),
    exit(normal).

run({'EXIT', _Graph, Reason}, _Node) ->
    exit(Reason);
run( {'DOWN', Ref, process, Neighbor, _Reason}, Node) ->
    erlang:demonitor(Ref, [flush]),
    rm_node_neighbor(Node, Neighbor);
run(_Msg, Node) ->
    % Default to doing nothing
    Node.

init(Id) ->
    #graph_node{id=Id}.

%% Private function

as_node(Node) ->
    #node{id = Node#graph_node.id, props = Node#graph_node.props}.

rm_node_neighbor(Node, Neighbor) ->
    Node#graph_node{neighbors=lists:delete(Neighbor, Node#graph_node.neighbors)}.

