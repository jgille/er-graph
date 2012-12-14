-module(graph_db_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../src/graph.hrl").

-define(FILE_NAME, "TestDb").

create_close_test_() ->
    {"The graph db can be created, closed and has registered info",
     {setup,
      fun create/0,
      fun close/1,
      fun(DbName) -> [check_size(DbName, 0)] end
     }
    }.

store_test_() ->
    {"We can store a node in the db.",
     {setup,
      fun create/0,
      fun close/1,
      fun(DbName) ->
              Ok = graph_db:store(DbName, #graph_node{id=1,
                                                      props=[{"Name", "Foo"}]}),
              [?_assertEqual(ok, Ok),
               check_size(DbName, 1)]
      end
     }
    }.

get_test_() ->
    Node = #graph_node{id=1, props=[{"Name", "Foo"}]},
    {"We can get a node from the db.",
     {setup,
      fun() ->
              DbName = create(),
              graph_db:store(DbName, Node),
              DbName
      end,
      fun close/1,
      fun(DbName) ->
              [?_assertEqual(Node, graph_db:lookup(DbName, 1)),
               ?_assertEqual(no_such_node, graph_db:lookup(DbName, 2))]
      end
     }
    }.

delete_test_() ->
    Node = #graph_node{id=1, props=[{"Name", "Foo"}]},
    {"We can delete a node from the db.",
     {setup,
      fun() ->
              DbName = create(),
              graph_db:store(DbName, Node),
              DbName
      end,
      fun close/1,
      fun(DbName) ->
              [?_assertEqual(ok, graph_db:delete(DbName, 1)),
               check_size(DbName, 0)]
      end
     }
    }.

create() ->
    graph_db:create(test_db, ?FILE_NAME),
    test_db.

close(DbName) ->
    graph_db:delete_all(DbName),
    graph_db:close(DbName).

check_size(DbName, Size) ->
    [?_assertEqual(Size, graph_db:size(DbName))].
