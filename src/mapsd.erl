%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Erlang Maps Dict API==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2016, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(mapsd).
-author('mjtruog [at] gmail (dot) com').

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
    M /= #{}.

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

