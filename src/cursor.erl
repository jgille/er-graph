%%%-------------------------------------------------------------------
%%% @author Jon Ivmark <jon@omega.avail.net>
%%% @copyright (C) 2012, Jon Ivmark
%%% @doc
%%%
%%% A module providing lazy manipulation of lists.
%%%
%%% @end
%%% Created : 27 Oct 2012 by Jon Ivmark <jon@omega.avail.net>
%%%-------------------------------------------------------------------
-module(cursor).

-export([from_list/1]).
-export([filter/2, map/2, reverse/1]).
-export([as_list/1, as_list/2, for_each/2, for_each/3, reduce/3, reduce/4,
         count/1, len/1, limit/2]).

%%% API functions

% Creates a cursor containing the elements in the list Xs.
from_list(Xs) ->
    {Xs, [], length(Xs)}.

% Applies the filter P to the cursor.
filter(P, {Xs, PipeLine, Limit}) ->
    {Xs, [{filter, P}|PipeLine], Limit}.

% Applies the map function F to the cursor.
map(F, {Xs, PipeLine, Limit}) ->
    {Xs, [{map, F}|PipeLine], Limit}.

% Creates a list containing all elements in the cursor.
as_list(Cur) ->
    as_list(Cur, -1).

% Creates a list containing a limited number of elements in the cursor.
as_list(Cursor, N) ->
    Result = reduce(fun(Xs, X) -> [X|Xs] end,
                    Cursor, [], N),
    lists:reverse(Result).

% Applies the function F to all elements in the cursor.
for_each(F, Cur) ->
    for_each(F, Cur, -1).

% Applies the function F to a limited number of elements in the cursor.
for_each(F, Cursor, N) ->
    reduce(fun(ok, Head) -> F(Head),
                            ok end,
           Cursor, ok, N).

% Performs a reduce/foldleft over all elements in the cursor.
reduce(F, Cur, Initial) ->
    reduce(F, Cur, Initial, -1).

% Performs a reduce/foldleft over a limited number of elements in the cursor.
reduce(Accumulator, {Xs, PipeLine, Limit}, Initial, N) ->
    recursive_reduce({Xs, lists:reverse(PipeLine), Limit}, Initial,
                     Accumulator, N).

% Gets the number of elements in the cursor.
count(Cur) ->
    reduce(fun(Cnt, _Any) -> Cnt + 1 end, Cur, 0).

% Gets the total length of the cursor, disregarding any filters,
% but taking the limit into account.
len({Xs, _PipeLine, Limit}) ->
    min(Limit, length(Xs)).

% Reverses the cursor.
reverse({Xs, PipeLine, Limit}) ->
    {lists:reverse(Xs), PipeLine, Limit}.

% Sets a limit on how many elements that will at most be
% traversed (including filtered elements).
limit(NewLimit, {Xs, PipeLine, _PrevLimit}) ->
    {Xs, PipeLine, NewLimit}.

% Private helper functions

cursor({Xs, PipeLine, Limit}) ->
    fun() -> next(Xs, PipeLine, Limit) end.

next(_Xs, _PipeLine, 0) ->
    done;
next(Xs, PipeLine, Limit) ->
    case map_first(Xs, PipeLine) of
        [] -> done;
        [filtered|T] -> next(T, PipeLine, Limit - 1);
        [H|T] -> {H, {T, PipeLine, Limit - 1}}
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
recursive_reduce({Xs, PipeLine, Limit}, Result, Accumulator, N) ->
    CursorFun = cursor({Xs, PipeLine, Limit}),
    Next = CursorFun(),
    case Next of
        done -> Result;
        {Head, NextCur} -> recursive_reduce(NextCur, Accumulator(Result, Head),
                                            Accumulator, N - 1)
    end.
