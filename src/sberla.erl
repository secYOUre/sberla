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
%% <h1>Elang Client of Google Safe Browsing APIv2</h1>
%% eCouch is an application that provides an API to a CouchDb server
%% It uses the <a href="http://www.lshift.net/blog/2007/02/17/json-and-json-rpc-for-erlang">rfc4627</a> module from <a href="http://www.lshift.net/">LShift</a>
%% The design was inspired in the article <a href="http://www.trapexit.org/Building_a_Non-blocking_TCP_server_using_OTP_principles">Building a Non-blocking TCP server using OTP principles</a>
%% and assumes that <a href="http://www.erlang.org/doc/apps/inets/index.html">inets</a> and ssl application are running.
%% todo:
%% Accept a list of servers and implement load distribution algorithms <br/>
%% Implement views
%%
%% @end
%% =====================================================================

-module(sberla).
-author('Alfonso De Gregorio').

-behaviour(application).


%% Application callbacks
-export([start/2, stop/1, init/1, start_client/0]).

%% API
-export([
         isItSafe/1,
         lookupURLs/1,
         requestMACKey/0
        ]).

-include("sberla.hrl").


-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
%% Define the timeout for gen_server calls to be something longer than 5 seconds.
-define(DEFAULT_TIMEOUT, 30000).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% @spec start(_Type, StartArgs::startargs()) -> {ok, Pid} |
%%                                 {ok, Pid, State} |
%%                                 {error, Reason}
%%
%% @type startargs() = {host(), tcp_port()}
%% @type host() = string()
%% @type tcp_port() = int()
%%
%% @doc This function is called whenever an application
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%% @end
%%--------------------------------------------------------------------

start(_Type, {SSLHost, SSLPort, Host, Port, Apikey}) ->
    case get_app_opt(sslhost, SSLHost) of
        none ->
            {error, "Missing required config option 'sslhost'"};
        SSLHostVal ->
            case get_app_opt(sslport, SSLPort) of
                none ->
                    {error, "Missing required config option 'sslport'"};
                SSLPortVal ->
                   case get_app_opt(host, Host) of
                       none ->
                           {error, "Missing required config option 'host'"};
                       HostVal ->
                          case get_app_opt(port, Port) of
                             none ->
                                 {error, "Missing required config option 'port'"};
                             PortVal ->
                                 case get_app_opt(user, Apikey) of
                                     none ->
                                         {error, "Missing required config options 'apikey'"};
                                     ApikeyVal ->
                                        supervisor:start_link({local, ?MODULE},
                                                              ?MODULE,
                                                              [SSLHostVal, 
                                                               SSLPortVal, 
                                                               Host, 
                                                               Port, 
                                                               ApikeyVal])
                                 end
                           end 
                    end
            end
    end.


%% @hidden

start_client() ->
    supervisor:start_child(sberla_client_sup, []).

%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------

stop(_State) ->
    ok.

%% @hidden

init([SSLHost, SSLPort, Host, Port, Apikey]) ->
    % making the init variable accessible through env (used by the tests)
    case application:get_application() of
        {ok, Application} ->
            application:set_env(Application, sslhost, SSLHost),
            application:set_env(Application, sslport, SSLPort),
            application:set_env(Application, host, Host),
            application:set_env(Application, port, Port),
            application:set_env(Application, apikey, Apikey);
        _ -> ok
    end,
    {ok,
        {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % sberla Listener
              {   sberla_listener_sup,   % Id       = internal id
                  {sberla_listener,start_link,[SSLHost, SSLPort, Host, Port, Apikey]},   % StartFun = {M, F, A}
                  permanent,  % Restart  = permanent | transient | temporary
                  2000,       % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,                  % Type     = worker | supervisor
                  [sberla_listener]        % Modules  = [Module] | dynamic
              },
              % Client instance supervisor
              {   sberla_client_sup,
                  {supervisor,start_link,[{local, sberla_client_sup}, ?MODULE, [sberla_client]]},
                  permanent, % Restart  = permanent | transient | temporary
                  infinity,  % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,  % Type     = worker | supervisor
			  []   % Modules  = [Module] | dynamic
              }
            ]
        }
    };

%% @hidden

init([Module]) ->
    {ok,
        {_SupFlags = {simple_one_for_one, ?MAX_RESTART, ?MAX_TIME},
            [
              % HTTP Client
              {   undefined,         % Id       = internal id
                  {Module,start_link,[]},  % StartFun = {M, F, A}
                  temporary, % Restart  = permanent | transient | temporary
                  2000,      % Shutdown = brutal_kill | int() >= 0 | infinity
                  worker,    % Type     = worker | supervisor
                  []         % Modules  = [Module] | dynamic
              }
            ]
        }
    }.

%% API Functions

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

get_api_options() ->
    [
      {"client", ?CLIENT},
      {"appver", ?APPVER},
      {"pver", ?PVER},
      {"apikey", get_app_opt(apikey, [])}
    ].

get_app_opt(Opt, Default) ->
    Value = case application:get_application() of
        {ok, Application} -> application:get_env(Application, Opt);
        _ -> undefined
        end,
    case Value of
        {ok, Val} -> Val;
        _ ->
            case init:get_argument(Opt) of
                [[Val | _]] -> Val;
                error -> Default
            end
        end.
