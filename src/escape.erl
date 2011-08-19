-module(escape).

-export([escape_uri/1, escape_byte/1, hex_digit/1]).

%% This is based on edoc_lib:escape_uri/1

escape_uri([C | Cs]) when C >= $a, C =< $z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $A, C =< $Z ->
    [C | escape_uri(Cs)];
escape_uri([C | Cs]) when C >= $0, C =< $9 ->
    [C | escape_uri(Cs)];
escape_uri([C = $. | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $- | Cs]) ->
    [C | escape_uri(Cs)];
escape_uri([C = $_ | Cs]) ->
    [C | escape_uri(Cs)];
%%escape_uri([C | Cs]) when C > 16#7f ->
%%    %% This assumes that characters are at most 16 bits wide.
%%    escape_byte(((C band 16#c0) bsr 6) + 16#c0)
%%        ++ escape_byte(C band 16#3f + 16#80)
%%        ++ escape_uri(Cs);
escape_uri([C | Cs]) ->
    escape_byte(C) ++ escape_uri(Cs);
escape_uri([]) ->
    [].


escape_byte(C) when C >= 0, C =< 255 ->
    [$%, hex_digit(C bsr 4), hex_digit(C band 15)].

hex_digit(N) when N >= 0, N =< 9 ->
    N + $0;
hex_digit(N) when N > 9, N =< 15 ->
   N + $a - 10.

