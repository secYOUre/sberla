{application, sberla,
 [
  {description, "(Google) Safe Browsing Erlang API"},
  {vsn, "0.1"},
  {id, "sberla"},
  {modules,      [sberla_listener, sberla_client]},
  {registered,   [sberla_client_sup, sberla_listener]},
  {applications, [kernel, stdlib, inets, ssl]},

  %%
  %% mod: Specify the module name to start the application, plus args
  %%
  {mod, {sberla, {"sb-ssl.google.com", "443", 
                  "safebrowsing.clients.google.com", "80",
                  "add-your-safebrowsing-api-key-here" }}},
  {env, []}
 ]
}.
