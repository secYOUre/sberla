%% =====================================================================
%% This library is free software; you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation; either version 2 of the
%% License, or (at your option) any later version.
%%
%% This library is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
%% Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public
%% License along with this library; if not, write to the Free Software
%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
%% USA
%%

%%%-------------------------------------------------------------------
%%% @private
%%% File:      sberla_client.erl
%%% @author    Alfonso De Gregorio <adg@crypto.lo.gy> []
%%% @copyright 2011 Alfonso De Gregorio
%%% @doc	based upon ecouch API by Vitor Rodrigues
%%%             https://github.com/greut/ecouch.git
%%%
%%% @end
%%%
%%% @since 2011-08-08 by Alfonso De Gregorio
%%%-------------------------------------------------------------------
-module(sberla_client).
-author('Alfonso De Gregorio').

-behaviour(gen_server).

%% API
-export([
         start_link/0, 
         query_string/1, 
         query_string/2, 
         url_encode/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% @spec init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% @doc Initiates the server
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, []}.

%%--------------------------------------------------------------------
%% @spec
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% @doc Handling call messages
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({Operation, Host, Port, Path, L, From}, State) ->
    case Operation of

        {get, Options} ->
            QueryString = lists:flatten(query_string(Options)),
            Url = lists:flatten(io_lib:format("https://~s:~s~s~s", [Host, Port, Path, QueryString])),
            Reply = http_g_request(Url),
            gen_server:reply(From, Reply),
            {stop, normal, State};

        {post, Options} ->
            QueryString = lists:flatten(query_string(Options)),
            Url = lists:flatten(io_lib:format("https://~s:~s~s~s", [Host, Port, Path, QueryString])),
            Body = format_body(L),
            Reply = http_p_request(post, Url, Body),
            gen_server:reply(From, Reply),
            {stop, normal, State};

        {put, _} ->
            gen_server:reply(From, {error, "Bad operation"}),
            {stop, normal, State};
        {delete, _} ->
            gen_server:reply(From, {error, "Bad operation"}),
            {stop, normal, State};
        _Other ->
            gen_server:reply(From, {error, "Bad operation"}),
            {stop, normal, State}
    end.

%%--------------------------------------------------------------------
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% @doc Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @spec terminate(Reason, State) -> void()
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

query_string(Options) ->
    query_string(Options, "?", []).
query_string(Key, Value)->
    query_string([{Key, Value}], "?", []).
query_string({Name, Value}, Separator, Acc) ->
    query_string([{Name, Value}], Separator, Acc);
query_string([{Name, Value} | T], Separator, Acc) when is_integer(Value) ->
    query_string([{Name, integer_to_list(Value)} | T], Separator, Acc);
query_string([{Name, Value} | T], Separator, Acc) ->
    UrlName = url_encode(lists:flatten(io_lib:format("~s", [Name]))),
    UrlValue = url_encode(lists:flatten(io_lib:format("~s", [Value]))),
    O = lists:flatten(io_lib:format("~s~s=~s", [Separator, UrlName, UrlValue])),
    query_string(T, "&", [O | Acc]);
query_string([], _Separator, Acc) ->
    lists:flatten(lists:reverse(Acc)).




format_body(L) ->
    format_body(L, "\n", [], 0).
format_body([], _Separator, Acc, C) ->
    lists:flatten(io_lib:format("~B~s", [C,lists:flatten(lists:reverse(Acc))]));
format_body([H|T], Separator, Acc, C) ->
    O = lists:flatten(io_lib:format("~s~s", [Separator, H])),
    format_body(T, Separator, [O | Acc], C + 1).


http_p_request(Method, Url, Body) ->
    http_p_request(Method, Url, Body, "text/plain").
http_p_request(Method, Url, Body, ContentType) ->
    case http:request(Method, {Url, [], ContentType, Body}, [{ssl, [{verify,1}]}], []) of
        {ok, {_Status, _Header, RespBody}} ->
            RespBody;
        {error, Reason} ->
            {error, Reason};
	{ok, Else} -> 
            case Else of 
               {_Status, _Header, Body} -> Body;
               E -> {stillunmatched, E}
            end
    end.
http_g_request(Url) ->
    case http:request(get, {Url, []}, [{ssl, [{verify,1}]}], []) of
        {ok, {_Status, _Header, Body}} ->
            %%{_Status, _Header, Body};
            Body;
        {error, Reason} ->
            {error, Reason}
    end.
http_d_request(Url) ->
    case http:request(delete, {Url, []}, [{ssl, [{verify,1}]}], []) of
        {ok, {_Status, _Header, Body}} ->
            Body;
        {error, Reason} ->
            {error, Reason}
    end.

url_encode(Url) -> edoc_lib:escape_uri(Url).
