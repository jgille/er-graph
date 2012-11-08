-module(graph_tests).

-include_lib("eunit/include/eunit.hrl").

-import(cursor, [as_list/1]).
-import(graph, [add/2, add/3, get/2, rm/2]).
-import(graph, [set_properties/3, add_neighbor/3, get_neighbors/2, rm_neighbor/3]).

start_stop_test_() ->
    {"The graph can be started, stopped and has a registered name",
     {setup,
      fun start/0,
      fun stop/1,
      fun is_registered/1
     }
    }.

add_test_() ->
    NodeId1 = "Node 1",
    {NodeId2, Properties2} = {"Node 2", [{"Age", 30}, {"Name", "Foo"}]},
    {"We can add a node to a graph",
     {setup,
      fun start/0,
      fun stop/1,
      fun(_Pid) ->
              [?_assertEqual({node, NodeId1, []}, add(my_graph, NodeId1)),
               ?_assertEqual({node, NodeId2, Properties2}, add(my_graph, NodeId2, Properties2))]
      end
     }
    }.

get_test_() ->
    NodeId1 = "Node 1",
    {NodeId2, Properties2} = {"Node 2", [{"Age", 30}, {"Name", "Foo"}]},
    {"We can get a node from a graph",
     {setup,
      fun() ->
              Pid = start(),
              add(my_graph, NodeId1),
              add(my_graph, NodeId2, Properties2),
              Pid
      end,
      fun stop/1,
      fun(_Pid) -> [?_assertEqual({node, NodeId1, []}, get(my_graph, NodeId1)),
                    ?_assertEqual({node, NodeId2, Properties2}, get(my_graph, NodeId2))] end
     }
    }.

rm_test_() ->
    NodeId = "Node 1",
    {"We can remove a node from a graph",
     {setup,
      fun() ->
              Pid = start(),
              add(my_graph, NodeId),
              Pid
      end,
      fun stop/1,
      fun(_Pid) ->
              [?_assertEqual(ok, rm(my_graph, NodeId)),
               ?_assertEqual(no_such_node, rm(my_graph, NodeId)),
               ?_assertEqual(no_such_node, get(my_graph, NodeId))] end
     }
    }.

rm_with_neigbors_test_() ->
    {NodeId1, Properties1} = {"Node 1", [{"Age", 30}, {"Name", "Foo"}]},
    {NodeId2, Properties2} = {"Node 2", [{"Age", 50}, {"Name", "Bar"}]},
    {NodeId3, Properties3} = {"Node 3", [{"Age", 40}, {"Name", "Baz"}]},
    {"We can remove a node from a graph and it is also removed in the corresponding neighbor lists",
     {setup,
      fun() ->
              Pid = start(),
              add(my_graph, NodeId1, Properties1),
              add(my_graph, NodeId2, Properties2),
              add(my_graph, NodeId3, Properties3),
              add_neighbor(my_graph, NodeId1, NodeId2),
              add_neighbor(my_graph, NodeId1, NodeId3),
              Pid
      end,
      fun stop/1,
      fun(_Pid) ->
              rm(my_graph, NodeId2),
              [?_assertEqual([{node, NodeId3, Properties3}],
                             as_list(get_neighbors(my_graph, NodeId1)))]
      end
     }
    }.

set_properties_test_() ->
    {NodeId, Properties} = {"Node 2", [{"Age", 30}, {"Name", "Foo"}]},
    {"We can set properties for a node in a graph",
     {setup,
      fun() ->
              Pid = start(),
              add(my_graph, NodeId),
              Pid
      end,
      fun stop/1,
      fun(_Pid) ->
              Node =  set_properties(my_graph, NodeId, Properties),
              [?_assertEqual({node, NodeId, Properties}, Node),
               ?_assertEqual({ok, 30}, node:find_property(Node, "Age")),
               ?_assertEqual(no_such_node, set_properties(my_graph, "I don't exist", Properties))]
      end
     }
    }.

add_neighbor_test_() ->
    {NodeId1, Properties1} = {"Node 1", [{"Age", 30}, {"Name", "Foo"}]},
    {NodeId2, Properties2} = {"Node 2", [{"Age", 50}, {"Name", "Bar"}]},
    {"We can add neighbors to a node in a graph",
     {setup,
      fun() ->
              Pid = start(),
              add(my_graph, NodeId1, Properties1),
              add(my_graph, NodeId2, Properties2),
              Pid
      end,
      fun stop/1,
      fun(_Pid) ->
              [?_assertEqual({node, NodeId1, Properties1}, add_neighbor(my_graph, NodeId1, NodeId2)),
               ?_assertEqual(no_such_node, add_neighbor(my_graph, NodeId1, "I don't exist")),
               ?_assertEqual(no_such_node, add_neighbor(my_graph, "I don't exist", NodeId2))]
      end
     }
    }.

get_neighbors_test_() ->
    {NodeId1, Properties1} = {"Node 1", [{"Age", 30}, {"Name", "Foo"}]},
    {NodeId2, Properties2} = {"Node 2", [{"Age", 50}, {"Name", "Bar"}]},
    {NodeId3, Properties3} = {"Node 3", [{"Age", 40}, {"Name", "Baz"}]},

    {"We can get neighbors for a node in a graph",
     {setup,
      fun() ->
              Pid = start(),
              add(my_graph, NodeId1, Properties1),
              add(my_graph, NodeId2, Properties2),
              add(my_graph, NodeId3, Properties3),
              add_neighbor(my_graph, NodeId1, NodeId2),
              add_neighbor(my_graph, NodeId1, NodeId3),
              Pid
      end,
      fun stop/1,
      fun(_Pid) ->
              [?_assertEqual([{node, NodeId3, Properties3}, {node, NodeId2, Properties2}],
                             as_list(get_neighbors(my_graph, NodeId1))),
               ?_assertEqual(no_such_node, get_neighbors(my_graph, "I don't exist"))]
      end
     }
    }.

rm_neighbors_test_() ->
    {NodeId1, Properties1} = {"Node 1", [{"Age", 30}, {"Name", "Foo"}]},
    {NodeId2, Properties2} = {"Node 2", [{"Age", 50}, {"Name", "Bar"}]},
    {NodeId3, Properties3} = {"Node 3", [{"Age", 40}, {"Name", "Baz"}]},

    {"We can remove neighbors from a node in a graph",
     {setup,
      fun() ->
              Pid = start(),
              add(my_graph, NodeId1, Properties1),
              add(my_graph, NodeId2, Properties2),
              add(my_graph, NodeId3, Properties3),
              add_neighbor(my_graph, NodeId1, NodeId2),
              add_neighbor(my_graph, NodeId1, NodeId3),
              Pid
      end,
      fun stop/1,
      fun(_Pid) ->
              [?_assertEqual({node, NodeId1, Properties1}, rm_neighbor(my_graph, NodeId1, NodeId2)),
               ?_assertEqual([{node, NodeId3, Properties3}],
                             as_list(get_neighbors(my_graph, NodeId1))),
               ?_assertEqual(no_such_node, rm_neighbor(my_graph, "I don't exist", NodeId2)),
               ?_assertEqual(no_such_node, rm_neighbor(my_graph, NodeId1, "I don't exist"))]
      end
     }
    }.

message_test_() ->
    {"The graph can receive strange messages and still carry on living",
     {setup,
      fun start/0,
      fun stop/1,
      fun(Pid) ->
              Msg = {1, 2},
              Pid ! Msg,
              [?_assert(erlang:is_process_alive(Pid))]
      end
     }
    }.

graph_server_test_() ->
    {"The graph server can receive strange messages and still carry on living",
     {setup,
      fun start/0,
      fun stop/1,
      fun(Pid) ->
              Msg = {1, 2},
              graph_server:run(Pid, Msg),
              [?_assert(erlang:is_process_alive(Pid))]
      end
     }
    }.

non_existing_graph_test_() ->
    {"A non existing graph causes errors",
     {setup,
      fun() -> ok end,
      fun(_Ignored) -> ok end,
      fun(_Ignored) ->
              Pid = list_to_pid("<0.239.0>"),
              Msg = {1, 2},
              ?_assertError(noproc, graph_server:call(Pid, Msg))
      end
     }
    }.

start() ->
    Pid = graph:start_link(),
    register(my_graph, Pid),
    Pid.

stop(Pid) ->
    unregister(my_graph),
    graph:stop(Pid).

is_registered(Pid) ->
    [?_assert(erlang:is_process_alive(Pid)),
     ?_assertEqual(Pid, whereis(my_graph))].
