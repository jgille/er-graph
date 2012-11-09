%%%-------------------------------------------------------------------
%%% @author Jon Ivmark <jon@omega.avail.net>
%%% @copyright (C) 2012, Jon Ivmark
%%% @doc
%%%
%%% A module providing lazy and chained mapping and filtering of lists.
%%%
%%% @end
%%% Created : 27 Oct 2012 by Jon Ivmark <jon@omega.avail.net>
%%%-------------------------------------------------------------------
-module(cursor).

-export([from_list/1]).
-export([filter/2, map/2, reverse/1]).
-export([as_list/1, as_list/2, for_each/2, for_each/3, reduce/3, reduce/4, count/1]).

%%% API functions

from_list(Xs) ->
    {Xs, []}.

filter(P, {Xs, PipeLine}) ->
    {Xs, [{filter, P}|PipeLine]}.

map(F, {Xs, PipeLine}) ->
    {Xs, [{map, F}|PipeLine]}.

as_list(Cur) ->
    as_list(Cur, -1).

as_list(Cursor, N) ->
    Result = reduce(fun(Xs, X) -> [X|Xs] end,
                    Cursor, [], N),
    lists:reverse(Result).

for_each(F, Cur) ->
    for_each(F, Cur, -1).

for_each(F, Cursor, N) ->
    reduce(fun(ok, Head) -> F(Head),
                            ok end,
           Cursor, ok, N).

reduce(F, Cur, Initial) ->
    reduce(F, Cur, Initial, -1).

reduce(Accumulator, {Xs, PipeLine}, Initial, N) ->
    recursive_reduce({Xs, lists:reverse(PipeLine)}, Initial, Accumulator, N).

count(Cur) ->
    reduce(fun(Cnt, _Any) -> Cnt + 1 end, Cur, 0).

reverse({Xs, PipeLine}) ->
    {lists:reverse(Xs), PipeLine}.

% Private helper functions

cursor({Xs, PipeLine}) ->
    fun() -> next(Xs, PipeLine) end.

next(Xs, PipeLine) ->
    case map_first(Xs, PipeLine) of
        [] -> done;
        [filtered|T] -> next(T, PipeLine);
        [H|T] -> {H, {T, PipeLine}}
    end.

map_first(Xs, []) ->
    Xs;
map_first([], _PipeLine) ->
    [];
map_first([filtered|T], _PipeLine) ->
    [filtered|T];
map_first([H|T], [F|Fs]) ->
    map_first([map_element(F, H)|T], Fs).

map_element({filter, P}, X) ->
    B = P(X),
    case B of
        true -> X;
        _ -> filtered
    end;
map_element({map, F}, X) ->
    F(X).

recursive_reduce(_Cursor, Result, _Accumulator, 0) ->
    Result;
recursive_reduce({Xs, PipeLine}, Result, Accumulator, N) ->
    CursorFun = cursor({Xs, PipeLine}),
    Next = CursorFun(),
    case Next of
        done -> Result;
        {Head, NextCur} -> recursive_reduce(NextCur, Accumulator(Result, Head), Accumulator, N - 1)
    end.
