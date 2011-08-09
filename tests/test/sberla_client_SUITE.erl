-module(sberla_client_SUITE).
-author('Alfonso De Gregorio').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(sberla_client).
-import(lists).
% Let's error out if our tests take over a minute to complete. This can be reconfigured
% on a per testcase basis in init_per_testcase.
suite() -> [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    Config.
    
end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
    
end_per_testcase(_TestCase, Config) ->
    Config.

all() -> [test_url_encode, test_query_string].

test_url_encode(_Config) ->
    ?line "%20" = sberla_client:url_encode(" "),
    ?line "%27" = sberla_client:url_encode("'"),
    ?line "%22" = sberla_client:url_encode("\""),
    ?line "%26" = sberla_client:url_encode("&"),
    ?line "%3d" = sberla_client:url_encode("="),
    ?line "%5c" = sberla_client:url_encode("\\"),
    ?line "%40" = sberla_client:url_encode("@"),
    ok.

test_query_string(_Config) ->
    ?line [] = sberla_client:query_string([]),
    ?line "?key=value" = sberla_client:query_string([{key, value}]),
    ?line "?key=value" = sberla_client:query_string([{"key", "value"}]),
    ?line "?key=value&key=value2" = sberla_client:query_string([{key, value}, {key, value2}]),
    ?line "?key=2" = sberla_client:query_string(key, 2),
    ?line "?key=hello%20world" = sberla_client:query_string(key, "hello world"),
    ?line "?key=%22apps%22" = sberla_client:query_string(key, "\"apps\""),
    ok.
