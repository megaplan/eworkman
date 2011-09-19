%%%
%%% eworkman_worker_web: start web server
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
%%% @since 2011-08-17 13:40
%%% @license MIT
%%% @doc start web server that serves the monitoring page
%%%

-module(eworkman_worker_web).

%%%----------------------------------------------------------------------------
%%% Exports
%%%----------------------------------------------------------------------------

-export([prepare_web/1, dispatch/2]).

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
%% @doc prepare web server which is used to serve worker monitoring page only
%% @since 2011-08-17 13:40
%%
-spec prepare_web(#ewm{}) -> #ewm{}.

prepare_web(C) ->
    case C#ewm.web_server_opts of
        [_|_] = List ->
            Docroot = proplists:get_value(docroot, List),
            Sconf = proplists:get_value(sconf, List),
            Gconf = proplists:get_value(gconf, List),
            Id = proplists:get_value(id, List),
            Res = yaws:start_embedded(Docroot, Sconf, Gconf, Id),
            C#ewm{web_server_pid = Res};
        _ ->
            C
    end.

%%-----------------------------------------------------------------------------
%%
%% @doc dispatcher for web requests
%% @since 2011-08-17 13:40
%%
-spec dispatch(any(), #ewm{}) -> any().

dispatch(Req, C) ->
    mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Req}, C#ewm.debug, http, 5),
    case Req:get(method) of
        'GET' ->
            Path = Req:get(path),
            Type = get_query_type(Req),
            mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Path, Type},
                C#ewm.debug, http, 5),
            get_resource(C, Req, Path, Type);
        _ ->
            Headers = [{"Allow", "GET"}],
            Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
    end.

%%%----------------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------------
%%
%% @doc gets status2 from eworkman_worker and sends it as plain text response
%% to the client of the web server
%%
-spec get_resource(#ewm{}, any(), any(), any()) -> any().

get_resource(C, Req, "/status2", "full") ->
    Res = eworkman_handler:get_status2(),
    mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Res}, C#ewm.debug, http, 4),
    Response = eworkman_worker_web_page:create_plain_status(Res),
    Req:ok(Response);
get_resource(C, Req, "/status2", _Type) ->
    Res = eworkman_handler:get_status2(),
    mpln_p_debug:pr({?MODULE, dispatch, ?LINE, Res}, C#ewm.debug, http, 4),
    Response = eworkman_worker_web_page:create_html_status(Res),
    Req:ok(Response);
get_resource(_C, Req, _Path, _Type) ->
    Req:respond({404, [], "404 Not Found\r\n"}).

%%-----------------------------------------------------------------------------
%%
%% @doc extracts query type from the request
%%
get_query_type(Req) ->
    Q = Req:parse_qs(),
    proplists:get_value("type", Q).

%%-----------------------------------------------------------------------------
