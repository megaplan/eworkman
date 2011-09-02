%%% 
%%% eworkman_worker_spawn: spawns one eworkman worker
%%%
%%% Copyright (c) 2011 Megaplan Ltd. (Russia)
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom
%%% the Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included
%%% in all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
%%% IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
%%% CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
%%% TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
%%% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
%%%
%%% @author arkdro <arkdro@gmail.com>
%%% @since 2011-07-15 10:00
%%% @license MIT
%%% @doc spawns one eworkman worker
%%% 

-module(eworkman_worker_spawn).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([spawn_one_worker/2, start_child/2]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-include("eworkman.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc Spawns a new worker, stores its pid (and a ref) in a list,
%% returns the modified pool.
%% @since 2011-07-21 18:00
%%
-spec spawn_one_worker(#ewm{}, #pool{}) -> {reference() | error, #pool{}}.

spawn_one_worker(C, Pool) ->
    Id = make_ref(),
    Workers = Pool#pool.workers,
    Res = start_child(Pool, Id),
    mpln_p_debug:pr({?MODULE, 'real_spawn_one_worker res', ?LINE, Res},
        C#ewm.debug, run, 3),
    case Res of
        {ok, Pid} ->
            Mref = erlang:monitor(process, Pid),
            Ch = #chi{pid=Pid, id=Id, start=now(), mon=Mref},
            {Id, Pool#pool{workers = [Ch | Workers]}};
        {ok, Pid, _Info} ->
            Mref = erlang:monitor(process, Pid),
            Ch = #chi{pid=Pid, id=Id, start=now(), mon=Mref},
            {Id, Pool#pool{workers = [Ch | Workers]}};
        {error, _Reason} ->
            {error, Pool}
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc creates child spec and calls supervisor to start child.
%% @since 2011-09-02 12:56
%%
-spec start_child(#pool{}, reference()) -> tuple().

start_child(Pool, Id) ->
    Child_config = make_child_config(Pool, Id),
    StartFunc = {eworkman_long_worker, start_link, [Child_config]},
    % for 'permanent' restart policy either worker or handler must contact
    % one another so handler keeps actual list of children. Or use gproc...
    % In the case of 'temporary' the handler does all the housekeeping
    Child = {Id, StartFunc, temporary, 1000, worker, [eworkman_long_worker]},
    supervisor:start_child(eworkman_long_supervisor, Child).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc creates config (proplist actually) for a child
%% @since 2011-07-21 18:00
%%
-spec make_child_config(#pool{}, reference()) -> list().

make_child_config(Pool, Ref) ->
    [{id, Ref} | Pool#pool.worker_config]
.
%%-----------------------------------------------------------------------------
