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

-module(sberla_sup).
-author('Alfonso De Gregorio').

-behaviour(supervisor).

%% API
-export([start_client/0, start_expression/0, stop/0]).

%% Supervisor callbacks
-export([init/1]).



-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).


%%====================================================================
%% API fucntions
%%====================================================================
start_client() ->
    supervisor:start_child(sberla_client_sup, []).

start_expression() ->
    supervisor:start_child(sberla_expression_sup, []).

stop() ->
    case whereis(sberla) of
        P when is_pid(P) ->
            exit(P, kill);
        _ -> ok
    end.
%%====================================================================
%% Supervisor callbacks
%%====================================================================

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
              % Expression subsystem instance supervisor
              {   sberla_expression_sup,
                  {supervisor,start_link,[{local, sberla_expression_sup}, ?MODULE, [sberla_expression]]},
                  permanent, % Restart  = permanent | transient | temporary
                  infinity,  % Shutdown = brutal_kill | int() >= 0 | infinity
                  supervisor,  % Type     = worker | supervisor
			  []   % Modules  = [Module] | dynamic
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
