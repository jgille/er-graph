-module(cursor_tests).

-include_lib("eunit/include/eunit.hrl").

-import(cursor, [start/1, as_list/1, as_list/2, for_each/2, for_each/3, filter/2, map/2]).

as_list_test_() ->
    [fun() -> test_as_list([]) end,
     fun() -> test_as_list([1]) end,
     fun() -> test_as_list(['A', 'B', 'C']) end,
     fun() -> test_as_list([{abc, 1, {'A', [1, 2, 3]}}, {bc}, [1, 2]]) end,
     fun() -> test_as_list([], 2) end,
     fun() -> test_as_list([1], 2) end,
     fun() -> test_as_list(['A', 'B', 'C'], 2) end,
     fun() -> test_as_list([{abc, 1, {'A', [1, 2, 3]}}, {bc}, [1, 2]], 2) end].

filter_test_() ->
    Filter = fun(X) -> X > 2 end,
    [fun() -> test_filter([], Filter) end,
     fun() -> test_filter([1, 2], Filter) end,
     fun() -> test_filter([1, 2, 3, 4, 1], Filter) end,
     fun() -> test_filter(['A', 2, 3, 4, 1], Filter) end].

map_test_() ->
    Map = fun(X) -> X * 2 end,
    [fun() -> test_map([], Map) end,
     fun() -> test_map([1, 2], Map) end,
     fun() -> test_map([1, 2, 3, 4, 1], Map) end,
     fun() -> ?assertError(badarith, as_list(map(Map, start(['A', 1])))) end].

filter_map_test_() ->
    Filter = fun(X) -> X > 2 end,
    Map = fun(X) -> X * 2 end,
    [fun() -> test_filter_map([], Filter, Map) end,
     fun() -> test_filter_map([1, 2, 3, 4, 1], Filter, Map) end].

map_filter_test_() ->
    Filter = fun(X) -> X > 2 end,
    Map = fun(X) -> X * 2 end,
    [fun() -> test_map_filter([], Map, Filter) end,
     fun() -> test_map_filter([1, 2, 3, 4, 1], Map, Filter) end].

for_each_test_() ->
    Filter = fun(X) -> X > 2 end,
    Map = fun(X) -> X * 2 end,
    [fun() -> test_for_each(start([])) end,
     fun() -> test_for_each(start([1, 2, 3])) end,
     fun() -> test_for_each(filter(Filter, start([1, 2, 3]))) end,
     fun() -> test_for_each(map(Map, start([1, 2, 3]))) end,
     fun() -> test_for_each(start([]), 2) end,
     fun() -> test_for_each(start([1, 2, 3]), 2) end,
     fun() -> test_for_each(filter(Filter, start([1, 2, 3])), 2) end,
     fun() -> test_for_each(map(Map, start([1, 2, 3])), 2) end].

test_as_list(Xs) ->
    Xs1 = as_list(start(Xs)),
    ?assertEqual(Xs, Xs1).

test_as_list(Xs, Limit) ->
    Xs1 = as_list(start(Xs), Limit),
    ?assertEqual(lists:sublist(Xs, Limit), Xs1).

test_filter(Xs, P) ->
    Xs1 = as_list(filter(P, start(Xs))),
    ?assertEqual(lists:filter(P, Xs), Xs1).

test_map(Xs, F) ->
    Xs1 = as_list(map(F, start(Xs))),
    ?assertEqual(lists:map(F, Xs), Xs1).

test_filter_map(Xs, P, F) ->
    Xs1 = as_list(map(F, filter(P, start(Xs)))),
    ?assertEqual(lists:map(F, lists:filter(P, Xs)), Xs1).

test_map_filter(Xs, F, P) ->
    Xs1 = as_list(filter(P, map(F, start(Xs)))),
    ?assertEqual(lists:filter(P, lists:map(F, Xs)), Xs1).

test_for_each(Cur) ->
    test_for_each(Cur, -1).

test_for_each(Cur, N) ->
    Mod = test_helper,
    meck:new(Mod),
    meck:expect(Mod, mock_me, fun(_X) -> ok end),
    fe(fun Mod:mock_me/1, Cur, N),
    Performed = meck:history(Mod),
    Xs = al(Cur, N),
    Expected = lists:map(fun(X) -> {self(), {Mod, mock_me, [X]}, ok} end, Xs),
    meck:unload(Mod),
    ?assertEqual(Expected, Performed).

fe(F, Cur, -1) ->
    for_each(F, Cur);
fe(F, Cur, N) ->
    for_each(F, Cur, N).

al(Cur, -1) ->
    as_list(Cur);
al(Cur, N) ->
    as_list(Cur, N).
