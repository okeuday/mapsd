%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Erlang Maps Dict API==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2016-2017 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(mapsd).
-author('mjtruog at protonmail dot com').

%% dict API external interface
-export([append/3,
         append_list/3,
         erase/2,
         fetch/2,
         fetch_keys/1,
         filter/2,
         find/2,
         fold/3,
         from_list/1,
         is_empty/1,
         is_key/2,
         map/2,
         merge/3,
         new/0,
         size/1,
         store/3,
         to_list/1,
         update/3,
         update/4,
         update_counter/3]).

%% maps API external interface
-export([get/2,
         get/3,
         keys/1,
         merge/2,
         put/3,
         remove/2,
         take/2,
         update_with/3,
         update_with/4,
         values/1,
         with/2,
         without/2]).

%% API conflicts (maps:update/3 conflicts with dict API external interface)
-export([update_value/3]).

%%%------------------------------------------------------------------------
%%% dict API external interface functions
%%%------------------------------------------------------------------------

-spec append(K :: any(),
             V :: any(),
             M :: map()) -> map().

append(K, V, M) ->
    maps:update_with(K, fun (L) -> L ++ [V] end, M).

-spec append_list(K :: any(),
                  Vs :: list(),
                  M :: map()) -> map().

append_list(K, Vs, M) ->
    maps:update_with(K, fun (L) -> L ++ Vs end, M).

-spec erase(K :: any(),
            M :: map()) -> map().

erase(K, M) ->
    maps:remove(K, M).

-spec fetch(K :: any(),
            M :: map()) -> any().

fetch(K, M) ->
    maps:get(K, M).

-spec fetch_keys(M :: map()) -> list().

fetch_keys(M) ->
    maps:keys(M).

-spec filter(F :: fun((any(), any()) -> boolean()),
             M :: map()) -> map().

filter(F, M) ->
    maps:filter(F, M).

-spec find(K :: any(),
           M :: map()) -> {ok, any()} | error.

find(K, M) ->
    maps:find(K, M).

-spec fold(F :: fun((any(), any(), any()) -> any()),
           A :: any(),
           M :: map()) -> any().

fold(F, A, M) ->
    maps:fold(F, A, M).

-spec from_list(L :: list()) -> map().

from_list(L) ->
    maps:from_list(L).

-spec is_empty(M :: map()) -> boolean().

is_empty(M) ->
    M == #{}.

-spec is_key(K :: any(),
             M :: map()) -> boolean().

is_key(K, M) ->
    maps:is_key(K, M).

-spec map(F :: fun((any(), any()) -> any()),
          M :: map()) -> map().

map(F, M) ->
    maps:map(F, M).

-spec merge(F :: fun((any(), any(), any()) -> any()),
            M1 :: map(),
            M2 :: map()) -> map().

merge(F, M1, M2) ->
    maps:fold(fun (K, V1, M) ->
                  maps:update_with(K, fun (V2) -> F(K, V1, V2) end, V1, M)
              end, M2, M1).

-spec new() -> map().

new() ->
    maps:new().

-spec size(M :: map()) -> non_neg_integer().

size(M) ->
    maps:size(M).

-spec store(K :: any(),
            V :: any(),
            M :: map()) -> map().

store(K, V, M) ->
    maps:put(K, V, M).

-spec to_list(M :: map()) -> list().

to_list(M) ->
    maps:to_list(M).

-spec update(K :: any(),
             F :: fun((any()) -> any()),
             M :: map()) -> map().

update(K, F, M) ->
    maps:update_with(K, F, M).

-spec update(K :: any(),
             F :: fun((any()) -> any()),
             V :: any(),
             M :: map()) -> map().

update(K, F, V, M) ->
    maps:update_with(K, F, V, M).

-spec update_counter(K :: any(),
                     I :: number(),
                     M :: map()) -> map().

update_counter(K, I, M) ->
    maps:update_with(K, fun (J) -> J + I end, I, M).

%%%------------------------------------------------------------------------
%%% maps API external interface functions
%%%------------------------------------------------------------------------

-spec get(K :: any(),
          M :: map()) -> any().

get(K, M) ->
    maps:get(K, M).

-spec get(K :: any(),
          M :: map(),
          D :: any()) -> any().

get(K, M, D) ->
    maps:get(K, M, D).

-spec keys(M :: map()) -> list().

keys(M) ->
    maps:keys(M).

-spec merge(M1 :: map(),
            M2 :: map()) -> map().

merge(M1, M2) ->
    maps:merge(M1, M2).

-spec put(K :: any(),
          V :: any(),
          M :: map()) -> map().

put(K, V, M) ->
    maps:put(K, V, M).

-spec remove(K :: any(),
             M :: map()) -> map().

remove(K, M) ->
    maps:remove(K, M).

-spec take(K :: any(),
           M :: map()) -> any().

take(K, M) ->
    maps:take(K, M).

-spec update_with(K :: any(),
                  F :: fun((any()) -> any()),
                  M :: map()) -> map().

update_with(K, F, M) ->
    maps:update_with(K, F, M).

-spec update_with(K :: any(),
                  F :: fun((any()) -> any()),
                  V :: any(),
                  M :: map()) -> map().

update_with(K, F, V, M) ->
    maps:update_with(K, F, V, M).

-spec values(M :: map()) -> list().

values(M) ->
    maps:values(M).

-spec with(Ks :: list(),
           M :: map()) -> map().

with(Ks, M) ->
    maps:with(Ks, M).

-spec without(Ks :: list(),
              M :: map()) -> map().

without(Ks, M) ->
    maps:without(Ks, M).

%%%------------------------------------------------------------------------
%%% API conflicts
%%%------------------------------------------------------------------------

-spec update_value(K :: any(),
                   V :: any(),
                   M :: map()) -> map().

update_value(K, V, M) ->
    maps:update(K, V, M).

