{application, esocketcho,
 [{vsn, "1.0.0"},
  {description, "Simple example a server"},
  {modules, [esocketcho, esocketcho_sup, esocketcho_serv]},
  {applications, [stdlib, kernel]},
  {registered, [esocketcho]},
  {mod, {esocketcho, []}},
  {env,
   [{max_restart, 3},
    {max_time, 60000},
    {port, 9009}]}
 ]}.
