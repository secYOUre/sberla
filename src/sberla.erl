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
%% $Id$
%%
%% @copyright 2011 Alfonso De Gregorio
%% @author Alfonso De Gregorio <adg@crypto.lo.gy>
%% @version {@version}
%%
%% @doc
%% <h1>Erlang Client to Google Safe Browsing supporting APIv2</h1>
%% sberla is an application that provides an Erlang API to 
%% Google Safe Browsing service.
%% sberla assumes that <a href="http://www.erlang.org/doc/apps/inets/index.html">inets</a> and ssl application are running.
%%
%% @end
%% =====================================================================

-module(sberla).
-author('Alfonso De Gregorio').

-export([start/0, start/1, stop/0]).


%% API
-export([
         isItSafe/1,
         lookupURLs/1,
         requestMACKey/0,
	 canon/1
        ]).

-include("sberla.hrl").


%% Define the timeout for gen_server calls to be something longer than 5 seconds
-define(DEFAULT_TIMEOUT, 30000).


%%--------------------------------------------------------------------
%% Function: start([, Type]) -> ok
%%
%%  Type =  permanent | transient | temporary
%%
%% Description: Starts the inets application. Default type
%% is temporary. see application(3)
%%--------------------------------------------------------------------
start() ->
    application:start(sberla).

start(Type) ->
    application:start(sberla, Type).

%%--------------------------------------------------------------------
%% Function: stop() -> ok
%%
%% Description: Stops the inets application.
%%--------------------------------------------------------------------
stop() ->
    application:stop(sberla).

%%--------------------------------------------------------------------
%% Function: connect(Host, Port, Options) ->
%%           connect(Host, Port, Options, Timeout -> ConnectionRef | {error, Reson}
%%
%%      Host - string()
%%      Port - integer()
%%      Options - [{Option, Value}]
%%      Timeout - infinity | integer().
%%
%% Description: Starts an ssh connection.
%%--------------------------------------------------------------------


%%====================================================================
%% API functions
%%====================================================================

%% Google Safe Browsing Lookup API
isItSafe(Url) ->
    %%BaseOptions = get_api_options(),
    %BaseOptions = [],
    Options = [ {"url", Url} ],
    Reply = gen_server:call(sberla_listener, 
                    {lookup_get, Options, ?SAFEBROWSING_PATH, []}, 
                    ?DEFAULT_TIMEOUT),
    handle_reply(Reply).


lookupURLs([H|T]) ->
    %% Options = get_api_options(),
    Options = [], 
    Reply = gen_server:call(sberla_listener, 
                    {lookup_post, Options, ?SAFEBROWSING_PATH, [H|T]},
                    ?DEFAULT_TIMEOUT),
    handle_reply(Reply).

%% Google Safe Browsing APIv2

requestMACKey() ->
    Reply = gen_server:call(sberla_listener,
                    {lookup_get, [], ?NEWKEY_PATH, []}, ?DEFAULT_TIMEOUT),
    %% TODO: update the environment with the mackey and the wrappedkey
    %%       make the keys available to the listener and keep'em in the 
    %%       listener state record
    handle_reply(Reply).


%% TODO
canon(Url) ->
   Reply = gen_server:call(sberla_listener, {canon, [], Url, []}, ?DEFAULT_TIMEOUT),
   Reply.

%%====================================================================
%% Internal functions
%%====================================================================

handle_reply(Reply) ->
    case Reply of
        {error, Reason} ->
            {error, Reason};
        "malware"         -> sberla_malware;
        "phishing"        -> sberla_phishing;
        "phishing,malwre" -> sberla_badware;
        "ok"              -> sberla_ok;
        ""                -> sberla_ok;
        U                 -> {unexpected, U}
        %%R                 -> io:format("~p ~n", [R])
    end.

