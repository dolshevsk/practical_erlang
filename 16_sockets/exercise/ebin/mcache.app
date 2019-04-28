{application, mcache,
 [
  {description, "mcache app"},
  {vsn, "0.0"},
  {modules, [mcache_app, mcache_sup, mcache_server]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {mcache_app, []}},
  {env, [
         {port, 1234},
         {numAcceptors, 10}
        ]}
 ]}.
