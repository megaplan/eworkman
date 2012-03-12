%%%
%%% eworkman_handler: gen_server that controls long-lasting workers
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
%%% @since 2011-08-29 14:00
%%% @license MIT
%%% @doc a gen_server that controls long-lasting workers
%%%

-module(eworkman_handler).
-behaviour(gen_server).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([start/0, start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([cmd2/2, cmd2/3]).
-export([add_pool/1, add_worker/1]).
-export([get_status2/0]).
-export([logrotate/0]).
-export([
    reload_config_signal/0,
    config_reload/0
    ]).

%%%----------------------------------------------------------------------------
%%% Includes
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("eworkman.hrl").

%%%----------------------------------------------------------------------------
%%% gen_server callbacks
%%%----------------------------------------------------------------------------
init(_) ->
    prepare_updated_config(),
    C = eworkman_conf:get_config_worker(),
    Ct = prepare_all(C),
    Conf_w = eworkman_worker_web:prepare_web(Ct),
    New = eworkman_worker_misc:prepare_workers(Conf_w),
    % trap_exit is unnecessary. Children are ripped by supervisor
    %process_flag(trap_exit, true),
    mpln_p_debug:pr({?MODULE, 'init done', ?LINE}, New#ewm.debug, run, 1),
    {ok, New, ?T}.
%%-----------------------------------------------------------------------------
%%
%% Handling call messages
%% @since 2011-08-29 11:00
%%
-spec handle_call(any(), any(), #ewm{}) ->
    {stop, normal, ok, #ewm{}}
    | {reply, any(), #ewm{}}.

%% @doc adds a new pool
handle_call({add_pool, Pool}, _From, St) ->
    New = eworkman_worker_misc:add_pool(St, Pool),
    {reply, ok, New};

handle_call(stop, _From, St) ->
    {stop, normal, ok, St};
handle_call(status2, _From, St) ->
    Res = get_status(St),
    {reply, Res, St};
handle_call(status, _From, St) ->
    {reply, St, St};
handle_call(_N, _From, St) ->
    mpln_p_debug:pr({?MODULE, 'other', ?LINE, _N}, St#ewm.debug, run, 2),
    {reply, {error, unknown_request}, St}.
%%-----------------------------------------------------------------------------
%%
%% Handling cast messages
%% @since 2011-08-29 11:00
%%
-spec handle_cast(any(), #ewm{}) -> any().

handle_cast(stop, St) ->
    {stop, normal, St};

handle_cast(config_reload, St) ->
    do_config_reload(St),
    {noreply, St};

handle_cast(reload_config_signal, St) ->
    New = process_reload_config(St),
    {noreply, New};

handle_cast(logrotate, St) ->
    prepare_log(St),
    {noreply, St};
handle_cast({add_worker, Pool_id}, St) ->
    mpln_p_debug:pr({?MODULE, 'cast add_worker', ?LINE, Pool_id},
        St#ewm.debug, run, 4),
    St_w = eworkman_worker_misc:throw_worker_pools(St, Pool_id),
    {noreply, St_w};
handle_cast(_N, St) ->
    mpln_p_debug:pr({?MODULE, 'cast other', ?LINE, _N}, St#ewm.debug, run, 2),
    {noreply, St}.
%%-----------------------------------------------------------------------------
%%
%% @doc Note: it won't be called unless trap_exit is set
%%
terminate(_, #ewm{pid_file=File} = State) ->
    yaws:stop(),
    eworkman_worker_misc:remove_workers(State),
    mpln_misc_run:remove_pid(File),
    mpln_p_debug:pr({?MODULE, 'terminate', ?LINE}, State#ewm.debug, run, 1),
    ok.
%%-----------------------------------------------------------------------------
%%
%% Handling all non call/cast messages
%% @since 2011-08-29 11:00
%%
-spec handle_info(any(), #ewm{}) -> {noreply, #ewm{}}.

handle_info({'DOWN', Mref, _, Oid, _Info}=Msg, State) ->
    mpln_p_debug:pr({?MODULE, info_down, ?LINE, Msg}, State#ewm.debug, run, 2),
    St_w = eworkman_worker_misc:handle_crashed(State, Mref, Oid),
    {noreply, St_w};
handle_info(timeout, State) ->
    mpln_p_debug:pr({?MODULE, info_timeout, ?LINE}, State#ewm.debug, run, 6),
    New = periodic_check(State),
    {noreply, New};

handle_info(periodic_check, State) ->
    mpln_p_debug:pr({?MODULE, periodic_check, ?LINE}, State#ewm.debug, run, 6),
    New = periodic_check(State),
    {noreply, New};

handle_info(_Req, State) ->
    mpln_p_debug:pr({?MODULE, other, ?LINE, _Req}, State#ewm.debug, run, 2),
    {noreply, State}.
%%-----------------------------------------------------------------------------
code_change(_Old_vsn, State, _Extra) ->
    {ok, State}.

%%%----------------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------------
-spec start() -> any().
%%
%% @doc starts handler gen_server
%% @since 2011-08-29 11:00
%%
start() ->
    start_link().

%%-----------------------------------------------------------------------------
-spec start_link() -> any().
%%
%% @doc starts handler gen_server with pre-defined config
%% @since 2011-08-29 11:00
%%
start_link() ->
    start_link(?CONF).

-spec start_link(string()) -> any().
%%
%% @doc starts handler gen_server with given config
%% @since 2011-08-29 11:00
%%
start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Config, []).

%%-----------------------------------------------------------------------------
-spec stop() -> any().
%%
%% @doc stops handler gen_server
%% @since 2011-08-29 11:00
%%
stop() ->
    gen_server:call(?MODULE, stop).

%%-----------------------------------------------------------------------------
%%
%% @doc calls any received command to be executed by long-lasting worker
%% @since 2011-07-22 12:00
%%
-spec cmd2(binary(), binary()) -> ok.

cmd2(Method, Url) ->
    cmd2(Method, Url, 5000).

%%
%% @doc calls any received command to be executed by long-lasting worker
%% with timeout defined
%% @since 2011-07-22 12:00
%%
-spec cmd2(binary(), binary(), non_neg_integer()) -> ok.

cmd2(Method, Url, Timeout) ->
    gen_server:call(?MODULE, {cmd2, Method, Url}, Timeout).

%%-----------------------------------------------------------------------------
%%
%% @doc adds a new pool into the server state. Input is a proplist of
%% {key, value} tuples.
%% @since 2011-08-03 15:04
%%
-spec add_pool(list()) -> ok.

add_pool(List) ->
    gen_server:call(?MODULE, {add_pool, List}).

%%-----------------------------------------------------------------------------
%%
%% @doc casts to add a new worker to the pool
%% @since 2011-08-16 13:19
%%
-spec add_worker(any()) -> ok.

add_worker(Pool_id) ->
    gen_server:cast(?MODULE, {add_worker, Pool_id}).

%%-----------------------------------------------------------------------------
%%
%% @doc gets status info from the server
%% @since 2011-08-17 14:33
%%
-spec get_status2() -> any().

get_status2() ->
    gen_server:call(?MODULE, status2).

%%-----------------------------------------------------------------------------
%%
%% @doc sends message to the server to rotate logs
%% @since 2011-09-13 17:29
%%
-spec logrotate() -> ok.

logrotate() ->
    gen_server:cast(?MODULE, logrotate).

%%-----------------------------------------------------------------------------
%%
%% @doc sends message to the server to reload config
%% @since 2012-02-10 14:29
%%
-spec config_reload() -> ok.

config_reload() ->
    gen_server:cast(?MODULE, config_reload).

%%
%% @doc send a message to the server to reload own config
%% @since 2012-03-01 12:17
%%
-spec reload_config_signal() -> ok.

reload_config_signal() ->
    gen_server:cast(?MODULE, reload_config_signal).

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc does miscellaneous periodic checks. E.g.: check for workers. Returns
%% updated state.
%%
-spec periodic_check(#ewm{}) -> #ewm{}.

periodic_check(#ewm{timer=Ref} = State) ->
    mpln_p_debug:pr({?MODULE, 'periodic_check', ?LINE},
                    State#ewm.debug, run, 5),
    mpln_misc_run:cancel_timer(Ref),
    Stl = check_log_rotate(State),
    Stw = eworkman_worker_misc:check_workers(Stl),
    Nref = erlang:send_after(?T, self(), periodic_check),
    Stw#ewm{timer=Nref}.

%%-----------------------------------------------------------------------------
%%
%% @doc rotate log if it's time to do it. Recreate log if it disappeared
%%
-spec check_log_rotate(#ewm{}) -> #ewm{}.

check_log_rotate(#ewm{log=Base, log_last=Last, log_rotate=Dur} = St) ->
    mpln_misc_log:recreate_log(Base),
    case mpln_misc_log:need_rotate(Last, Dur) of
        true ->
            prepare_log(St),
            St#ewm{log_last = calendar:local_time()};
        false ->
            St
    end.

%%-----------------------------------------------------------------------------
get_status(St) ->
    eworkman_worker_misc:get_process_info(St).

%%-----------------------------------------------------------------------------
%%
%% @doc prepare all the things
%%
-spec prepare_all(#ewm{}) -> #ewm{}.

prepare_all(C) ->
    prepare_log(C),
    write_pid(C),
    Ref = erlang:send_after(?T, self(), periodic_check),
    C#ewm{timer=Ref}.

%%-----------------------------------------------------------------------------
%%
%% @doc prepare log if it is defined
%%
-spec prepare_log(#ewm{}) -> ok.

prepare_log(#ewm{log=undefined}) ->
    ok;
prepare_log(#ewm{log=Log, delay_for_log=T}) ->
    timer:sleep(T),
    mpln_misc_log:prepare_log(Log).

%%-----------------------------------------------------------------------------
%%
%% @doc writes pid file
%% @since 2011-11-11 14:17
%%
-spec write_pid(#ewm{}) -> ok.

write_pid(#ewm{pid_file=undefined}) ->
    ok;
write_pid(#ewm{pid_file=File}) ->
    mpln_misc_run:write_pid(File).

%%-----------------------------------------------------------------------------
%%
%% @doc reads config file and sets parameters of (configured) applications
%% to new values
%%
do_config_reload(#ewm{local_config=C} = St) ->
    case file:consult(C) of
        {ok, [Data]} ->
            erpher_conf:add_config(Data),
            proceed_config(St, Data);
        Other ->
            mpln_p_debug:pr({?MODULE, 'do_config_reload error', ?LINE, C,
                Other}, St#ewm.debug, run, 0)
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc sets new env values for applications stored in config
%%
proceed_config(#ewm{apps=Apps} = St, Conf) ->
    F = fun({A, _}) ->
        Procs = proplists:get_value(A, Apps, []),
        send_reload(St, Procs)
    end,
    lists:foreach(F, Conf).

%%-----------------------------------------------------------------------------
%%
%% @doc sends reload signal to processes
%%
send_reload(St, Names) ->
    Res = [catch X:reload_config_signal() || X <- Names],
    mpln_p_debug:pr({?MODULE, 'send_reload', ?LINE, Names, Res},
        St#ewm.debug, run, 2).

%%-----------------------------------------------------------------------------
%%
%% @doc fetches config from updated environment and stores it in the state
%%
-spec process_reload_config(#ewm{}) -> #ewm{}.

process_reload_config(St) ->
    yaws:stop(),
    %eworkman_worker_misc:remove_workers(St), % ???
    mpln_misc_run:remove_pid(St#ewm.pid_file),

    C = eworkman_conf:get_config_worker(St),
    Ct = prepare_all(C),
    Conf_w = eworkman_worker_web:prepare_web(Ct),
    New = eworkman_worker_misc:prepare_workers(Conf_w),

    mpln_p_debug:pr({?MODULE, 'process_reload_config done', ?LINE},
        New#ewm.debug, run, 1),
    New.

%%-----------------------------------------------------------------------------
prepare_updated_config() ->
    C = eworkman_conf:get_config_worker(),
    case file:consult(C#ewm.local_config) of
        {ok, [Data]} ->
            erpher_conf:add_config(Data);
        Other ->
            mpln_p_debug:pr({?MODULE, 'prepare_updated_config error', ?LINE,
                             Other}, C#ewm.debug, run, 0)
    end.

%%-----------------------------------------------------------------------------
