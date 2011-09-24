-module(sberla_api_SUITE).
-author('Alfonso De Gregorio').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(test_helper).
-import(sberla).
-import(lists).
% Let's error out if our tests take over a minute to complete. This can be reconfigured
% on a per testcase basis in init_per_testcase.
suite() -> [{timetrap, {minutes, 1}}].

true_negatives() -> [
	          "http://www.google.com/",
                  "http://www.irtf.org/",
                  "http://www.ietf.org/",
                  "http://www.iana.org/"
                   ].

true_positives() -> ["live.com.google.com.baidu-msn.com.bestartsale.ru"].
 
alleged_false_negatives() -> ["http://17ebook.com",
"http://aladel.net",
"http://bpwhamburgorchardpark.org",
"http://clicnews.com",
"http://dfwdiesel.net",
"http://divineenterprises.net",
"http://fantasticfilms.ru",
"http://gardensrestaurantandcatering.com",
"http://ginedis.com",
"http://gncr.org",
"http://hdvideoforums.org",
"http://hihanin.com",
"http://kingfamilyphotoalbum.com",
"http://likaraoke.com",
"http://mactep.org",
"http://magic4you.nu",
"http://marbling.pe.kr",
"http://nacjalneg.info",
"http://pronline.ru",
"http://purplehoodie.com",
"http://qsng.cn",
"http://seksburada.net",
"http://sportsmansclub.net",
"http://stock888.cn",
"http://tathli.com",
"http://teamclouds.com",
"http://texaswhitetailfever.com",
"http://wadefamilytree.org",
"http://xnescat.info",
"http://yt118.com"].


init_per_suite(Config) ->
    application:start(inets),
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    application:start(sberla),
    Config.
    
end_per_suite(_Config) ->
    application:stop(sberla),
    application:stop(ssl),
    application:stop(public_key),
    application:stop(crypto),
    application:stop(inets),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
    
end_per_testcase(_TestCase, Config) ->
    Config.

all() -> [
          test_get_interface,
          test_post_interface,
          test_true_negatives, 
          test_true_positives, 
          test_alleged_false_negatives
         ].

test_get_interface() ->
    [{userdata,
        {doc, "Testing Lookup API: get-interface."}}].
test_get_interface(_Config) ->
    % Now test.
    ?line sberla_ok = sberla:isItSafe("http://www.google.com/"),
    ok.

test_post_interface() ->
    [{userdata,
        {doc, "Testing Lookup API: post-interface."}}].
test_post_interface(_Config) ->
    % Now test.
    ?line sberla_ok = sberla:lookupURLs(["http://www.google.com/"]),
    ok.

test_true_negatives() ->
    [{userdata,
        {doc, "Testing Lookup API: true-negatives."}}].
test_true_negatives(_Config) ->
    % Now test.
    ?line sberla_ok = sberla:lookupURLs(true_negatives()),
    ok.

test_true_positives() ->
    [{userdata,
        {doc, "Testing Lookup API: true-positives."}}].
test_true_positives(_Config) ->
    % Now test.
    %% the target URL does not contain malware anymore, look further
    %% ?line sberla_malware = sberla:lookupURLs(true_positives()),
    ?line sberla_ok = sberla:lookupURLs(true_positives()),
    ok.

test_alleged_false_negatives() ->
    [{userdata,
        {doc, "Testing Lookup API: alleged-true-negatives."}}].
test_alleged_false_negatives(_Config) ->
    % Now test.
    ?line sberla_ok = sberla:lookupURLs(alleged_false_negatives()),
    ok.
