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

-export([as_list/1, as_list/2, for_each/2, for_each/3, start/1, filter/2, map/2]).

start(Xs) ->
    {Xs, []}.

filter(P, {Xs, PipeLine}) ->
    {Xs, [{filter, P}|PipeLine]}.

map(F, {Xs, PipeLine}) ->
    {Xs, [{map, F}|PipeLine]}.

cursor({Xs, PipeLine}) ->
    fun() -> next(Xs, PipeLine) end.

next(Xs, PipeLine) ->
    Next = step(Xs, PipeLine),
    case Next of
        [] -> done;
        [filtered|T] -> next(T, PipeLine);
        [H|T] -> {H, {T, PipeLine}}
    end.

as_list(Cur) ->
    as_list(Cur, -1).
as_list(Cur, N) ->
    as_list(Cur, [], N).

as_list({Xs, PipeLine}, Res, N) ->
    rec_as_list({Xs, lists:reverse(PipeLine)}, Res, N).

rec_as_list(_, Res, 0) ->
    lists:reverse(Res);
rec_as_list({Xs, PipeLine}, Res, N) ->
    C = cursor({Xs, PipeLine}),
    Next = C(),
    case Next of
        done -> lists:reverse(Res);
        {E, NextCur} -> rec_as_list(NextCur, [E|Res], N - 1)
    end.

for_each(F, Cur) ->
    for_each(F, Cur, -1).

for_each(F, {Xs, PipeLine}, N) ->
    rec_for_each(F, {Xs, lists:reverse(PipeLine)}, N).

rec_for_each(_, _, 0) ->
    ok;
rec_for_each(F, {Xs, PipeLine}, N) ->
    C = cursor({Xs, PipeLine}),
    Next = C(),
    case Next of
        done -> ok;
        {E, NextCur} -> F(E),
                        rec_for_each(F, NextCur, N - 1)
    end.

step(Xs, []) ->
    Xs;
step([], _) ->
    [];
step([filtered|T], _) ->
    [filtered|T];
step([H|T], [Instr|Rem]) ->
    step([do_map(Instr, H)|T], Rem).

do_map({filter, P}, X) ->
    B = P(X),
    case B of
        true -> X;
        _ -> filtered
    end;
do_map({map, F}, X) ->
    F(X).


