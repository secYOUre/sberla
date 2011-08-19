

-define(RE_HEX, "^0x([a-fA-F0-9]+)$").  %% match hex quantities
-define(RE_OCT, "^0([0-7]+)$").	        %% match octal quantities
-define(RE_DEC, "^(\\d+)$").            %% match decimal quantities
-define(RE_IP_WITH_TRAILING_SPACE, "^(\\d{1,3}\.\\d{1,3}\.\\d{1,3}\.\\d{1,3}) ").
-define(RE_POSSIBLE_IP, "^(?i)((?:0x[0-9a-f]+|[0-9.])+)$").
-define(RE_FIND_BAD_OCTAL, "(^|\.)0\\d*[89]").
-define(RE_HOST_PORT, "^(?:.*@)?(?P<host>[^:]*)(:(?P<port>\\d+))?$").

-define(RE_REPLACE_CHARS, "[\t\r\n]").
-define(RE_SPLIT_SEPARATOR, "[.]").
-define(RE_SPLIT_PATH, "%2f").

