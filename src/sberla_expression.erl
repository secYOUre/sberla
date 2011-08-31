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
%%% File:      sberla_expression.erl
%%% @author    Alfonso De Gregorio <adg@crypto.lo.gy> 
%%% @copyright 2011 Alfonso De Gregorio
%%% @doc
%%%
%%% @end
%%%
%%% @since 2011-08-08 by Alfonso De Gregorio
%%%-------------------------------------------------------------------
-module(sberla_expression).
-author('Alfonso De Gregorio').

-behaviour(gen_server).

%% API
-export([
         start_link/0
        ]).

%% -export([url_netloc/1, url_netloc_re/2, canonicalize_url/2, canonicalize_host/2, canonicalize_ip/2, canonicalize_path/2, unquote/1, unescape/1, unquote_recurse/2, nstr/3, url_split/1, process_ip_part/2, process_ip_part/3, get_octets/1, ends_with/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).



-include("sberla_expression.hrl").


-record(state, {   re_hex, 
                   re_oct, 
                   re_dec, 
                   re_ip_with_trailing_space, 
                   re_possible_ip, 
                   re_find_bad_octal,
                   re_host_port,
                   re_replace_chars,
                   re_split_separator,
                   re_split_path
               }).


%%--------------------------------------------------------------------
%% macro definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).


-define(PERCENT, 37).  % $\%
-define(IS_HEX(C), ((C >= $0 andalso C =< $9) orelse
                    (C >= $a andalso C =< $f) orelse
                    (C >= $A andalso C =< $F))).


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
    {ok, HexRe} = re:compile(?RE_HEX),
    {ok, OctRe} = re:compile(?RE_OCT),
    {ok, DecRe} = re:compile(?RE_DEC),
    {ok, IpTrailRe} = re:compile(?RE_IP_WITH_TRAILING_SPACE),
    {ok, PossibleIpRe} = re:compile(?RE_POSSIBLE_IP),
    {ok, FindBadOctalRe} = re:compile(?RE_FIND_BAD_OCTAL),
    {ok, HostPortRe} = re:compile(?RE_HOST_PORT),
    {ok, ReplaceCharsRe} = re:compile(?RE_REPLACE_CHARS),
    {ok, SplitSeparatorRe} = re:compile(?RE_SPLIT_SEPARATOR),
    {ok, SplitPathRe} = re:compile(?RE_SPLIT_PATH),

    {ok, #state{           re_hex = HexRe,
                           re_oct = OctRe,
                           re_dec = DecRe,
        re_ip_with_trailing_space = IpTrailRe,
                   re_possible_ip = PossibleIpRe,
                re_find_bad_octal = FindBadOctalRe,
                     re_host_port = HostPortRe,
                 re_replace_chars = ReplaceCharsRe,
               re_split_separator = SplitSeparatorRe,
                    re_split_path = SplitPathRe
              }}.


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
    Reply = State,   %% TODO for testing only: to be removed when ready
    %% Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% @doc Handling cast messages
%% @end
%%--------------------------------------------------------------------
handle_cast({Operation, Arg, From}, State) ->
    case Operation of
        canonicalize_url  ->
            Reply = canonicalize_url(Arg, State),
            gen_server:reply(From, Reply),
            {stop, normal, State};

        _Other ->
            gen_server:reply(From, {error, "Not implemented"}),
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

%%
%% Return Url in Google Safe Browsing canonical form
%%
canonicalize_url(Url, State) ->
    %% Start by stripping off the fragment identifier
    SharpPos = string:chr(Url, $#),
    if SharpPos  > 0 -> Url2 = string:substr(Url, 1, SharpPos -1 );
       SharpPos =< 0 -> Url2 = Url
    end,

    %% Stripping off leading and trailing white spaces
    Url3 = string:strip(Url2, both),

    %% Remove any embedded tabs and CR/LF characters which aren't escaped
    Url4 = re:replace(Url3, State#state.re_replace_chars, "", 
                                              [{return, list}, global]),

    %% Un-escape and re-escape the URL just in case there are some encoded
    %% characters in the url scheme for example.
    Url5 = unescape(Url4),

    UrlSplit = url_split(Url5),

    %% Split the Url, looking at the URI scheme
    case url_scheme(UrlSplit) of
        UrlScheme when   (UrlScheme =:= "http") 
                  orelse (UrlScheme =:= "https")
                  orelse (UrlScheme =:= "ftp")  ->
            %% Note: user and password, if any, are also discarded
	    [Host, Port] = url_netloc_re(UrlSplit, State),

            %% canonicalize the host part
            CanonicalHost = canonicalize_host(string:to_lower(Host), State),

            SPort = get_standard_port(UrlScheme),
            if Port =/= SPort andalso Port =/= [] -> 
                  CanonicalHost2 = CanonicalHost ++ ":" ++ Port;
               Port =:= SPort orelse Port =:= [] ->
                  CanonicalHost2 = CanonicalHost
            end,
            CanonicalPath = canonicalize_path(url_path(UrlSplit), State),

            Query = url_query(UrlSplit),
            Qmark = url_endswith_qmark(UrlSplit),

            if Query =/= [] orelse Qmark ->
                  _CanonicalUrl = url_scheme(UrlSplit) ++
                                 ":\/\/" ++
                                 CanonicalHost2 ++
                                 CanonicalPath ++
                                 "?" ++ url_query(UrlSplit);
              Query == [] andalso not Qmark ->
                  _CanonicalUrl = url_scheme(UrlSplit) ++
                                 ":\/\/" ++
                                 CanonicalHost2 ++
                                 CanonicalPath
             end;

        _UnvalidScheme -> []
    end.

    
url_encode(Url) -> escape:escape_uri(Url).


%% Fully unescape the given string, then re-escape once.
%% Returns an escape string according to the SafeBrowsing protocol,
%% actually to a stricter escaping criteria.
unescape(Unescaped) ->
    Unquoted = unquote(Unescaped),
    url_encode(unquote_recurse(Unquoted, Unescaped)).

unquote_recurse(Unquoted, Unescaped) ->
	case Unquoted of
		Unescaped -> Unquoted;
		_         -> unquote_recurse(unquote(Unquoted), Unquoted)
	end.

%%
%% Split the Url in its scheme, netloc, path, query
%% Return [Scheme Netloc, Path, Query, EndWithPM] where EndWithPm is
%% a boolean true if the Url ends with '?'
%% 
url_split(Url) ->
    %%SS        = string:str(Url, ":\/\/"),
    SS        = string:str(Url, "%3a%2f%2f"),
    %%PathStart = nchr(Url, $\/, 3),
    Length    = length(Url),
    %%ParamMark = string:str(Url, "?"),
    %% EndWithPM = ( "?" =:= string:substr(Url, Length)),
    EndWithPM = ( "%3f" =:= string:substr(Url, Length - 2)),
    if SS =:= 0 -> 
             Scheme = "http", 
             NetlocStart = 1,
             PathSlash = nstr(Url, "%2f", 1);
       SS =/= 0 -> 
             Scheme = string:substr(Url, 1, SS -1), 
             NetlocStart = SS+9,
             PathSlash = nstr(Url, "%2f", 3)
    end,
    ParamMark = string:str(Url, "%3f"),
    if ParamMark =/= 0 -> PathEnd = ParamMark;
       ParamMark =:= 0 -> PathEnd = Length + 1
    end,
    if PathSlash =:= 0 -> 
                          PathStart = Length + 1,
                          NetlocLen  = PathStart - NetlocStart;
       PathSlash =/= 0 -> 
                          PathStart = PathSlash,
                          NetlocLen = PathStart - NetlocStart - 3
    end,
    Netloc    = string:substr(Url, NetlocStart, NetlocLen),
    if PathStart >= Length ->
           Path = [], Query = [];
       PathStart < Length ->
           Path      = string:substr(Url, PathStart, PathEnd - PathStart),
           if ParamMark =/= 0 ->
                %% Query = string:substr(Url, PathEnd+1, Length-PathEnd ),
                Query = string:substr(Url, ParamMark +3, Length - PathEnd+3);
              ParamMark =:= 0 ->
                Query = []
            end
    end,
    [Scheme, Netloc, Path, Query, EndWithPM].

%%
%% Return the the URL scheme from Url, defaults to "http"
%% 
url_scheme(UrlSplit) ->
%%    case string:substr(Url, 1, string:str(Url, ":\/\/") - 1) of
    case lists:nth(1, UrlSplit) of
        []    -> "http";
	Other -> string:to_lower(Other)
    end.

%%
%% Return the netloc part from the Url, 
%% leaving username and password if present
%%
url_netloc(Url) ->
    NetlocStart = string:str(Url, ":\/\/") + 3,
    NetlocEnd   = nchr(Url, $\/, 3),
    string:substr(Url, NetlocStart, NetlocEnd - NetlocStart).

%%
%% Return the netloc part of the Url. 
%% Username and password, if any, are removed
%%
url_netloc_re(UrlSplit, State) ->
    {match, [Host, Port]}=re:run(unquote(lists:flatten(lists:nth(2, UrlSplit))),
                            State#state.re_host_port,   
                            [{capture, ['host', 'port'], list}]
                            ),
     [url_encode(Host), url_encode(Port)].

url_path(UrlSplit) ->
    lists:nth(3, UrlSplit).

url_query(UrlSplit) ->
    unquote(lists:nth(4, UrlSplit)).

url_endswith_qmark(UrlSplit) ->
    lists:nth(5, UrlSplit).



%%
%% Return the N-th occurrence of Char in String.
%%
nchr(String, Char, N) ->
    nchr(String, Char, N, 0, 1).
nchr(String, Char, N, Index, C) ->
    if
        C  > N ->  Index;
        C =< N ->
                  Nc = string:chr(String, Char),
                   S = string:substr(String, Nc + 1),
                   nchr(S, Char, N, Index + Nc, C + 1)
    end.


%%
%% Return the N-th occurrence of Mark in String
%%
nstr(String, Mark, N) ->
    nstr(String, Mark, N, 1, 0).
nstr(String, Mark, N, Index, C) ->
    if
        C >= N ->  Index ;
        C < N ->
                  Ns = string:str(String, Mark),
                  if Ns =:= 0 -> 
                              nstr(String, Mark, N, 0, N);
                     Ns =/= 0 -> 
                              S = string:substr(String, Ns + length(Mark)),
                              nstr(S, Mark, N, Index + Ns + length(Mark) -1, C + 1)
                  end
    end.

%%
%% Return the standard port for the given URI Scheme
%%
get_standard_port(Scheme) ->
    case Scheme of 
        "http"  -> 80;
        "https" -> 443;
        "ftp"   -> 21;
         _      -> []
    end.


%%
%% Return the canonical form of the given hostname
%%
canonicalize_host(Host, State) ->
    EscapedHost = unescape(string:to_lower(Host)),
    Ip = canonicalize_ip(EscapedHost, State),
    case Ip of 
        [] ->
            %% Host is a normal hostname
            %% Skip trailing, leading and consecutive dots.
            HostSplit = lists:filter(fun(X) -> X =/= [] end, 
                           re:split(Host, State#state.re_split_separator, 
                                                          [{return, list}])),
            L = length(HostSplit),
            if L <  2 -> CanonicalHost = Host; %% XXX single part host
               L >= 2 -> CanonicalHost = string:join(HostSplit, ".")
            end;
        _Addr -> 
            CanonicalHost = Ip
    end,
    CanonicalHost.



%%
%% Return the canonical form of the given IP address
%%
canonicalize_ip(Ip, State) ->
    L = length(Ip),
    if L =< 15 ->
           %% The Windows resolver allows a 4-part dottet deciman IP address
           %% to have a space followed by any old rubbish, so long as the
           %% total length of the string doesn't get above 15 characters. So, 
           %% "10.192.95.89 xy" is resolved to 10.192.95.89.
           %% If the string length is greater than 15 characters
           %% e.g., "10.192.95.89 xy.wildcard.example.com", it will be 
           %% resolved through DNS.

%%State#state.re_ip_with_trailing_space,

           case re:run(Ip, "^(\\d{1,3}\.\\d{1,3}\.\\d{1,3}\.\\d{1,3}) $",
                                                 [{capture, [1], list}]) of
               {match, M} -> Host = M;
               nomatch    -> Host = Ip
           end;
       L > 15 -> Host = Ip
    end,
    %% Try to match a possible IP
    case re:run(Host, State#state.re_possible_ip, [{capture, [1], list}]) of
        {match, [IpM]} ->
            %% Basically we should parse octal if we can, but if there are
            %% illegal octal numbers, i.e. 08 or 09, then we should just
            %% look at deciman and hex
            AllowOctal = (nomatch =:= re:run(IpM, State#state.re_find_bad_octal)),

            %% Skip trailing, leading and consecutive dots.
            HostSplit = lists:filter(fun(X) -> X =/= [] end, 
                              re:split(Ip, State#state.re_split_separator, 
                                                         [{return, list}])),
            SplitLength = length(HostSplit),
            if SplitLength >  4 -> 
                   CanonicalIp = [];
               SplitLength =< 4 ->
                   IpParts = [ process_ip_part(X, AllowOctal, State) || 
                               X <- lists:sublist(HostSplit, 1,  SplitLength-1)
                             ],
                   Bytes = get_octets(
                             list_to_integer(
                                lists:flatten(
                                 lists:sublist(HostSplit, SplitLength, 1)
                                             )
                                            )
                                     ),
                   %% Octets = lists:filter(fun(X) -> X =/= 0 end, Bytes),
                   Octets = lists:sublist(Bytes, 4 - length(IpParts)),
                 
                   TotalLength = length(IpParts) + length(Octets),
                   if TotalLength  > 4 -> CanonicalIp=[];
                      TotalLength =< 4 -> CanonicalIp = lists:append(IpParts, 
                                                        lists:reverse(Octets))
                   end
            end;
        nomatch ->
            CanonicalIp = []
    end,
    string:join([integer_to_list(X) || X <- CanonicalIp], ".").


process_ip_part(X, State) ->
    process_ip_part(X, true, State).
process_ip_part(X, AllowOctal, State) ->
    %% string_to_decimal will automatically match the applicable base
    Value = string_to_decimal(X, AllowOctal, State),
    %% If greater than 255 mask the value to 0xff in order to make it
    %% a valid IP octect
    if Value > 255  -> Value band 16#ff;
       Value =< 255 -> Value
    end.

%% Parse the String as a number in base Base and return it in decimal
%% string_to_decimal/1 guess the base from the number syntax.
%% An empty list is returned if no base conversion can be performed.
string_to_decimal(String, State) when is_list(String) ->
    string_to_decimal(String, true, State);
string_to_decimal(_String, _State) -> [].

string_to_decimal(String, AllowOctal, State)   when is_list(String) 
                                     andalso is_atom(AllowOctal) ->
    {Base, Match} = find_base(String, AllowOctal, State),
    case {Base, Match} of
         {nobase, nomatch} -> [];
         {_, _}            -> erlang:list_to_integer(
                                        lists:flatten(Match), Base)
         %% {_, _}            -> string_to_decimal(Match, integer_to_list(Base), State)
    end;
string_to_decimal(_, _, _) -> [].



%% Return {Base, Match} where Base is the base of the number represented
%% by String and Match the matched text. {nobase, nomatch} is returned
%% if the base is unknown
find_base(String, AllowOctal, State) ->
    case re:run(String, State#state.re_hex, [{capture, [1], list}]) of
        {match, Match} -> Base = 16;
        nomatch        ->
          case re:run(String, State#state.re_oct, [{capture, [1], list}]) of
              {match, Match} -> Base = 8;
              nomatch        ->
                  case re:run(String, State#state.re_dec, [{capture, [1], list}]) of
                      {match, Match} -> Base = 10;
                      nomatch        -> Match = nomatch, Base = nobase
                  end
          end
    end,
    if
       %% if string looks like an octal number, but this base is not allowed
       %% let's consider it to be decimal, being the only one compatible here
       not AllowOctal andalso Base =:= 8 -> {10, Match};
       AllowOctal orelse Base =/= 8     -> {Base, Match}
    end.



get_octets(N) when is_integer(N) ->
  get_octets(N, 4, 0, []).
get_octets(N, L, C, Bytes) when is_integer(N) ->
   if N =< 0 andalso C >= L -> Bytes;
     N  > 0 orelse  C < L ->
      B  = lists:append(Bytes, [N band 16#ff]),
      get_octets(trunc(N/256), L, C+1, B)
  end.

%%
%% Return the canonical form of the given pathname
%%
canonicalize_path(Path, State) ->
    case Path of
      [] -> "/";
      "%2f" -> "/";
      _P   ->
        %% There are some cases where the path will not start with '/'.
        %% Example "ftp://host.com?g" -- the hostname is host.com and the
        %% path '%3Fq'.
        %% Browsers typically do prepend a leading slash to the path in
        %% this case, we'll do the same.
        %%FirstChar = lists:nth(1, Path),
        FirstChar = string:substr(Path, 1, 3),
        if (FirstChar =/= "%2f" ) -> PathP = "%2f" ++ Path;
           (FirstChar =:= "%2f" ) -> PathP = Path
        end,
        UnescapedPath = unescape(PathP),
        PathComponents = process_pathcomponents(
                   re:split(UnescapedPath, State#state.re_split_path, 
                                                      [{return, list}] ) ),
                   %% re:split(UnescapedPath, "[\/]", [{return, list}] ) ),
    
 
        %% Put the path components back together and re-add the leading slash
        %% which got stripped by removing empty path components.
        NC = length(PathComponents),
        if NC =/= 0 ->
            CanonicalPath = "%2f" ++ string:join(PathComponents, "%2f");
           NC =:= 0 ->
            CanonicalPath = "%2f" 
        end,
 
        %% CheckPath = ends_with(Path, 47), 
        CheckPath = ends_with(Path, "%2f"),
        CheckPath2 = ends_with(Path, $\/),
        CheckCanonical = ends_with(CanonicalPath, "%2f"),
        CheckCanonical2 = ends_with(CanonicalPath, $\/),

        if (CheckPath  andalso not CheckCanonical) orelse 
           (CheckPath2 andalso not CheckCanonical2) ->
                Result = CanonicalPath ++ "%2f";

           (not CheckPath orelse CheckCanonical) orelse
           (not CheckPath2 orelse CheckCanonical2)  ->
                Result = CanonicalPath
        end,
        NR = encode_path(unquote(Result)),
        NR
    end.

process_pathcomponents(P) ->
    lists:foldl(fun(X, C) -> case X of
                                  %% We skip empty path components to remove
                                  %% successive slashes (i.e., // -> /).
                                  %% Note: this means that the leading and
                                  %% trailing slash will als obe removed and
                                  %% need to be re-added afterwards.
                                  %%
                                  %% If the path component is "." we also skip
                                  %% it (i.e., /./ -_> /).
                                  "."  -> C;
                                  ""   -> C;
                                  %% If the path component is ".." we skip 
                                  %% it and remove the preceding path
                                  %% component if there are any.

                                  ".." -> lists:delete(lists:last(C), C);
                                  N    -> lists:append(C, [N])
                             end end, [], P).

    


encode_path([H|T]) ->
  if 
        H == $\   ->
            [$%, $2, $0 | encode_path(T)];
        H == $#   ->
            [$%, $2, $3 | encode_path(T)];
        H == $%   ->
            [$%, $2, $5 | encode_path(T)];
        H =/= $\  ->
            [H | encode_path(T)]
  end;
encode_path([]) ->
  [].


ends_with(S, A) when is_list(A) ->
    Al = length(A), Sl = length(S),
    if Sl >= Al ->
                string:substr(S, Sl-Al+1, Al) =:= A;
       Sl <  Al -> 
                false
    end;
ends_with(S, A) ->
    lists:nth(length(S), S) =:= A.




%%
%% The functions below are Copyright (c) 2007 Mochi Media, Inc.
%%



unhexdigit(C) when C >= $0, C =< $9 -> C - $0;
unhexdigit(C) when C >= $a, C =< $f -> C - $a + 10;
unhexdigit(C) when C >= $A, C =< $F -> C - $A + 10.

%% @spec unquote(string() | binary()) -> string()
%% @doc Unquote a URL encoded string.
unquote(Binary) when is_binary(Binary) ->
    unquote(binary_to_list(Binary));
unquote(String) ->
    qs_revdecode(lists:reverse(String)).

qs_revdecode(S) ->
    qs_revdecode(S, []).

qs_revdecode([], Acc) ->
    Acc;
qs_revdecode([$+ | Rest], Acc) ->
    qs_revdecode(Rest, [$\s | Acc]);
qs_revdecode([Lo, Hi, ?PERCENT | Rest], Acc) when ?IS_HEX(Lo), ?IS_HEX(Hi) ->
    qs_revdecode(Rest, [(unhexdigit(Lo) bor (unhexdigit(Hi) bsl 4)) | Acc]);
qs_revdecode([C | Rest], Acc) ->
    qs_revdecode(Rest, [C | Acc]).
