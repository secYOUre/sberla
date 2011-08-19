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

-module(sberla_app).
-author('Alfonso De Gregorio').

-behaviour(application).


%% Application callbacks
-export([start/2, stop/1]).



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

                                        supervisor:start_link({local, sberla_sup},
                                                              sberla_sup,
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


%%--------------------------------------------------------------------
%% @spec stop(State) -> void()
%% @doc This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored.
%% @end
%%--------------------------------------------------------------------

stop(_State) ->
    sberla_sup:stop(),
    ok.


%% ###########################################################
%% Internal functions
%% ###########################################################

%% @hidden
 
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

