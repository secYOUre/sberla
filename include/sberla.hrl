-define(CLIENT, "sberla"). %% client identity
-define(APPVER, "0.1").    %% client version
-define(PVER, "3.0").      %% protocol version supported by sberla

%% Paths accessed over SSL
-define(SAFEBROWSING_PATH, "/safebrowsing/api/lookup"). % SB Lookup API Path
-define(NEWKEY_PATH, "/safebrowsing/newkey"). % MAC Key Request (over SSL)


%% Paths accessed over plain HTTP
-define(MACREQUEST_PATH, "/safebrowsing/downloads"). % Requesting the MAC
-define(LISTREQUEST_PATH, "/safebrowsing/list"). % HTTP Request for List
-define(DATAREQUEST_PATH, "/safebrowsing/downloads"). %HTTP Request for Data
-define(FULLHASH_PATH, "/safebrowsing/gethash"). % Req. for Full-Length Hashes
