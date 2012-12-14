%%% @author Jon Ivmark <jon@omega.avail.net>
%%% @copyright (C) 2012, Jon Ivmark
%%% @doc
%%%
%%% A graph database backed by a DETS table.
%%%
%%% @end
%%% Created : 14 Nov 2012 by Jon Ivmark <jon@omega.avail.net>

-module(graph_db).

-export([create/2, close/1,
         store/2, lookup/2, delete/2, delete_all/1,
         size/1]).

-include("graph.hrl").

create(DbName, FileName) ->
    dets:open_file(DbName, [{file, FileName}, {keypos, #graph_node.id}]).

close(DbName) ->
    dets:close(DbName).

store(DbName, #graph_node{id=_Id, props=_Props, neighbors=_Neighbors} = Node) ->
    dets:insert(DbName, Node).

lookup(DbName, NodeId) ->
    case dets:lookup(DbName, NodeId) of
        [] -> no_such_node;
        [Node] -> Node
    end.

delete(DbName, NodeId) ->
    dets:delete(DbName, NodeId).

delete_all(DbName) ->
    dets:delete_all_objects(DbName).

size(DbName) ->
    Info = dets:info(DbName),
    [_Type, _KeyPos, {size, Size}, _FileSize, _FileName] = Info,
    Size.

