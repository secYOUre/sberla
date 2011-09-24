-module(sberla_canonicalization_SUITE).
-author('Alfonso De Gregorio').

-compile(export_all).

-include_lib("common_test/include/ct.hrl").

-import(test_helper).
-import(sberla).
-import(lists).
% Let's error out if our tests take over a minute to complete. This can be reconfigured
% on a per testcase basis in init_per_testcase.
suite() -> [{timetrap, {minutes, 1}}].

init_per_suite(Config) ->
    application:start(inets),
    application:start(ssl),
    application:start(sberla),
    Config.

end_per_suite(_Config) ->
    application:stop(sberla),
    application:stop(ssl),
    application:stop(inets),
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.
    
end_per_testcase(_TestCase, Config) ->
    Config.

all() -> [test_canonicalize].

test_canonicalize(_Config) ->
    ?line "http://host/%25" = sberla:canon("http://host/%25%32%35"),
   ?line "http://host/%25%25" = sberla:canon("http://host/%25%32%35%25%32%35"),
    ?line "http://host/%25" = sberla:canon("http://host/%2525252525252525"),
    ?line "http://host/asdf%25asd" = sberla:canon("http://host/asdf%25%32%35asd"),
    ?line "http://host/%25%25%25asd%25%25" = sberla:canon("http://host/%%%25%32%35asd%%"),
    ?line "http://www.google.com/" = sberla:canon("http://www.google.com/"),
    ?line "http://168.188.99.26/.secure/www.ebay.com/" = sberla:canon("http://%31%36%38%2e%31%38%38%2e%39%39%2e%32%36/%2E%73%65%63%75%72%65/%77%77%77%2E%65%62%61%79%2E%63%6F%6D/"),

    ?line "http://195.127.0.11/uploads/%20%20%20%20/.verify/.eBaysecure=updateuserdataxplimnbqmn-xplmvalidateinfoswqpcmlx=hgplmcx/" = sberla:canon("http://195.127.0.11/uploads/%20%20%20%20/.verify/.eBaysecure=updateuserdataxplimnbqmn-xplmvalidateinfoswqpcmlx=hgplmcx/"),
%%    ?line "http://host%23.com/~a!b@c%23d$e%25f^00&11*22(33)44_55+" = sberla:canon("http://host%23.com/%257Ea%2521b%2540c%2523d%2524e%25f%255E00%252611%252A22%252833%252944_55%252B"),
    ?line "http://host%23.com/~a!b@c%23d$e%25f^00&11*22(33)44_55%20" = sberla:canon("http://host%23.com/%257Ea%2521b%2540c%2523d%2524e%25f%255E00%252611%252A22%252833%252944_55%252B"),
    ?line "http://195.127.0.11/blah" = sberla:canon("http://3279880203/blah"),
    ?line "http://www.google.com/" = sberla:canon("http://www.google.com/blah/.."),
    ?line "http://www.google.com/" = sberla:canon("www.google.com/"),
    ?line "http://www.google.com/" = sberla:canon("www.google.com"),
    ?line "http://www.evil.com/blah" = sberla:canon("http://www.evil.com/blah#frag"),
    ?line "http://www.google.com/" = sberla:canon("http://www.GOOgle.com/"),
    ?line "http://www.google.com/" = sberla:canon("http://www.google.com.../"),
    ?line "http://www.google.com/foobarbaz2" = sberla:canon("http://www.google.com/foo\tbar\rbaz\n2"),
    ?line "http://www.google.com/q?" = sberla:canon("http://www.google.com/q?"),
    ?line "http://www.google.com/q?r?" = sberla:canon("http://www.google.com/q?r?"),
    ?line "http://www.google.com/q?r?s" = sberla:canon("http://www.google.com/q?r?s"),
    ?line "http://evil.com/foo" = sberla:canon("http://evil.com/foo#bar#baz"),
    ?line "http://evil.com/foo;" = sberla:canon("http://evil.com/foo;"),
    ?line "http://evil.com/foo?bar;" = sberla:canon("http://evil.com/foo?bar;"),
    ?line "http://%01%80.com/" = sberla:canon("http://\x01\x80.com/"),
    ?line "http://notrailingslash.com/" = sberla:canon("http://notrailingslash.com"),
    ?line "http://www.gotaport.com:1234/" = sberla:canon("http://www.gotaport.com:1234/"),
    ?line "http://www.google.com/" = sberla:canon("  http://www.google.com/  "),
    ?line "http://%20leadingspace.com/" = sberla:canon("http:// leadingspace.com/"),
    ?line "http://%20leadingspace.com/" = sberla:canon("http://%20leadingspace.com/"),
    ?line "http://%20leadingspace.com/" = sberla:canon("%20leadingspace.com/"),
    ?line "https://www.securesite.com/" = 	sberla:canon("https://www.securesite.com/"),
    ?line "http://host.com/ab%23cd" = 	sberla:canon("http://host.com/ab%23cd"),
    ?line "http://host.com/twoslashes?more//slashes" = sberla:canon("http://host.com//twoslashes?more//slashes"),
    ok.
