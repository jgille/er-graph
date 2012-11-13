%%% @author Jon Ivmark <jon@omega.avail.net>
%%% @copyright (C) 2012, Jon Ivmark
%%% @doc
%%%
%%% A module that handles individual nodes in a graph.
%%%
%%% @end
%%% Created : 19 Oct 2012 by Jon Ivmark <jon@omega.avail.net>

-module(node).

-export([start_link/1, get/1, find_property/2, set_properties/2]).
-export([get_neighbors/2, add_neighbor/3, rm_neighbor/3, stop/1]).
-export([call/3, run/2, init/1]).

-include("graph.hrl").

%%% Client API

start_link(Id) -> graph_server:start_link(?MODULE, Id).

get(Pid) ->
    graph_server:call(Pid, get).

set_properties(Pid, Props) ->
    graph_server:call(Pid, {set_properties, orddict:from_list(Props)}).

get_neighbors(Pid, EdgeType) ->
    graph_server:call(Pid, {get_neighbors, EdgeType}).

add_neighbor(Pid, Neighbor, EdgeType) ->
    graph_server:call(Pid, {add_neighbor, Neighbor, EdgeType}).

rm_neighbor(Pid, Neighbor, EdgeType) ->
    graph_server:call(Pid, {rm_neighbor, Neighbor, EdgeType}).

find_property(Node, Key) ->
    Props = Node#node.props,
    orddict:find(Key, Props).

stop(Pid) ->
    graph_server:call(Pid, stop).

%%% Server functions

call(get, Caller, Node) ->
    graph_server:reply(Caller, as_node(Node)),
    Node;

call({set_properties, Props}, Caller, Node) ->
    NewNode = Node#graph_node{props=Props},
    graph_server:reply(Caller, as_node(NewNode)),
    NewNode;

call({get_neighbors, EdgeType}, Caller, Node) ->
    Neighbors = nbs(Node, EdgeType),
    graph_server:reply(Caller, Neighbors),
    Node;

call({add_neighbor, Neighbor, EdgeType}, Caller, Node) ->
    erlang:monitor(process, Neighbor),
    Neighbors = nbs(Node, EdgeType),
    AllNeighbors = Node#graph_node.neighbors,
    NewNode = Node#graph_node{neighbors=orddict:store(EdgeType,
                                                      [Neighbor|Neighbors],
                                                      AllNeighbors)},
    graph_server:reply(Caller, as_node(NewNode)),
    NewNode;

call({rm_neighbor, Neighbor, EdgeType}, Caller, Node) ->
    Neighbors = nbs(Node, EdgeType),
    AllNeighbors = Node#graph_node.neighbors,
    NewNode =
        Node#graph_node{neighbors= orddict:store(EdgeType,
                                                 lists:delete(Neighbor,
                                                              Neighbors),
                                                 AllNeighbors)},
    graph_server:reply(Caller, as_node(NewNode)),
    NewNode;

call(stop, Caller, _) ->
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

nbs(Node, EdgeType) ->
    AllNeighbors = Node#graph_node.neighbors,
    Neighbors = orddict:find(EdgeType, AllNeighbors),
    case Neighbors of
        error -> [];
        {ok, Xs} -> Xs
    end.

rm_node_neighbor(Node, Neighbor) ->
    AllNeighbors = Node#graph_node.neighbors,
    Fun = fun(_EdgeType, NeighborList) ->
                  lists:delete(Neighbor, NeighborList)
          end,
    Node#graph_node{neighbors=orddict:map(Fun, AllNeighbors)}.

