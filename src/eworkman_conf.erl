%%%
%%% eworkman_conf: functions for config
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
%%% @doc eworkman functions related to config file read, config processing
%%%

-module(eworkman_conf).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([get_config/0]).
-export([get_config_worker/0]).
-export([fill_one_pool_config/1]).
-export([get_config_child/1]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eworkman.hrl").

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
%%
%% @doc fills in the child (a worker, actually) config with values from
%% the input list
%% @since 2011-08-31 12:18
%%
-spec get_config_child(list()) -> #child{}.

get_config_child(List) ->
    #child{
        name = proplists:get_value(name, List),
        id = proplists:get_value(id, List),
        debug = proplists:get_value(debug, List, [])
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file, fills in ewm record with configured values
%% @since 2011-08-29 14:09
%%
-spec get_config_worker() -> #ewm{}.

get_config_worker() ->
    List = get_config_list(),
    fill_ewm_worker_config(List).

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file for receiver, fills in ewm record with configured
%% values
%% @since 2011-07-15
%%
-spec get_config() -> #ewm{}.

get_config() ->
    List = get_config_list(),
    fill_config(List).

%%-----------------------------------------------------------------------------
%%
%% @doc gets data from the list of key-value tuples and stores it into
%% ewm record
%% @since 2011-07-15
%%
-spec fill_config(list()) -> #ewm{}.

fill_config(List) ->
    #ewm{
        debug = proplists:get_value(debug, List, []),
        log = proplists:get_value(log, List, ?LOG)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc fills config for one pool
%%
fill_one_pool_config(List) ->
    #pool{
        id = proplists:get_value(id, List),
        worker_config = proplists:get_value(worker, List, []),
        workers = [],
        w_duration = get_worker_duration(List),
        restart_policy = proplists:get_value(restart_policy, List),
        restart_delay = proplists:get_value(restart_delay, List, 10),
        min_workers = proplists:get_value(min_workers, List, 2)
    }.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc gets duration for some tag
%% @since 2011-09-02 19:10
%%
-spec get_tag_duration(any(), list()) -> non_neg_integer().

get_tag_duration(Tag, List) ->
    get_tag_duration(Tag, List, 0).

-spec get_tag_duration(any(), list(), non_neg_integer()) -> non_neg_integer().

get_tag_duration(Tag, List, Default) ->
    case proplists:get_value(Tag, List, Default) of
        Val when is_integer(Val), Val > 0 ->
            Val;
        _ ->
            0
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc gets worker duration from the list
%% @since 2011-08-25 17:50
%%
get_worker_duration(List) ->
    get_tag_duration(worker_duration, List, 86400).

%%-----------------------------------------------------------------------------
%%
%% @doc chooses either file from application config or default file and then
%% does read_config for that file
%% @since 2011-08-01 17:01
%%
-spec get_config_list() -> list().

get_config_list() ->
    application:get_all_env('eworkman').

%%-----------------------------------------------------------------------------
%%
%% @doc fills configs for the pools defined
%%
fill_pools_config(List) ->
    Pools = proplists:get_value(pools, List, []),
    lists:map(fun fill_one_pool_config/1, Pools)
.

%%-----------------------------------------------------------------------------
%%
%% @doc creates a worker config
%% @since 2011-08-29 14:15
%%
-spec fill_ewm_worker_config(list()) -> #ewm{}.

fill_ewm_worker_config(List) ->
    Pools = fill_pools_config(List),
    Web = fill_web_config(List),
    Log = fill_log_config(List, Web),
    Log#ewm{
        w_pools = Pools,
        debug = proplists:get_value(debug, List, [])
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc fills config with log related parameters
%%
fill_log_config(List, C) ->
    C#ewm{
        log = proplists:get_value(log, List),
        log_last = calendar:local_time(),
        log_rotate = proplists:get_value(log_rotate, List),
        delay_for_log = get_tag_duration(delay_for_log, List)
    }.

%%-----------------------------------------------------------------------------
%%
%% @doc gets web server parameters and stores them in the config record
%%
fill_web_config(List) ->
    #ewm{
        web_server_opts = proplists:get_value(web_server_opts, List, [])
    }.

%%%----------------------------------------------------------------------------
%%% EUnit tests
%%%----------------------------------------------------------------------------
-ifdef(TEST).
fill_config_test() ->
    #ewm{debug=[], log=?LOG} = fill_config([]),
    #ewm{debug=[{info, 5}, {run, 2}], log=?LOG} =
    fill_config([
        {debug, [{info, 5}, {run, 2}]}
        ]).

%%-----------------------------------------------------------------------------
get_test_config_2() ->
[

{pools, [
    [
        {id, p4},
        {min_workers, 1}, % long lasting workers
        {restart_policy, delay},
        {restart_delay, 10}, % sec. Delay before restarting the crashed worker
        {worker_duration, 601}, % seconds. Time before terminate
        {worker, [
            {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
            {debug,
                [
                    {run, 6},
                    {http, 1}
                ]
            }]}
    ],
    [
        {id, p1},
        {min_workers, 1}, % long lasting workers
        {restart_policy, delay},
        {restart_delay, 10}, % sec. Delay before restarting the crashed worker
        {worker_duration, 0}, % seconds. Time before terminate
        {worker, [
            {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
            {debug,
                [
                    {run, 6},
                    {http, 1}
                ]
            }]}
    ],
    [
        {id, p3},
        {min_workers, 1}, % long lasting workers
        {restart_policy, delay},
        {restart_delay, 10}, % sec. Delay before restarting the crashed worker
        {worker, [
            {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
            {debug,
                [
                    {run, 6},
                    {http, 1}
                ]
            }]}
    ],
    [
        {id, p2},
        {min_workers, 2}, % long lasting workers
        {worker_duration, -1}, % seconds. Time before terminate
        {worker, [
            {name, "/etc/erpher/workers/test.sh"},
            {debug,
                [
                    {run, 6},
                    {http, 1}
                ]
            }]}
    ]
]}

].

%%-----------------------------------------------------------------------------
fill_ewm_handler_config_test() ->
    Config = get_test_config_2(),
    C = fill_ewm_worker_config(Config),
    C2 = #ewm{
        w_pools = [
            #pool{
                id=p4,
                min_workers=1,
                restart_policy=delay,
                restart_delay=10,
                w_duration=601,
                worker_config= [
                    {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
                    {debug,
                        [
                            {run, 6},
                            {http, 1}
                        ]
                    }]
            },
            #pool{
                id=p1,
                min_workers=1,
                restart_policy=delay,
                restart_delay=10,
                w_duration=0,
                worker_config=[
                    {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
                    {debug,
                        [
                            {run, 6},
                            {http, 1}
                        ]
                    }]
            },
            #pool{
                id=p3,
                min_workers=1,
                restart_policy=delay,
                restart_delay=10,
                worker_config=[
                    {name, "/usr/bin/perl -Mstrict -w /etc/erpher/workers/t.pl"},
                    {debug,
                        [
                            {run, 6},
                            {http, 1}
                        ]
                    }]
            },
            #pool{
                id=p2,
                min_workers=2,
                w_duration=0,
                restart_delay=10,
                worker_config=[
                    {name, "/etc/erpher/workers/test.sh"},
                    {debug,
                        [
                            {run, 6},
                            {http, 1}
                        ]
                    }]
            }
        ]
    },
    ?assert(C#ewm.w_pools =:= C2#ewm.w_pools).

-endif.
%%-----------------------------------------------------------------------------
