{application, etalk,
 [{vsn, "1.0.0"},
  {description, "Simple example gen_event"},
  {modules, [etalk]},
  {applications, [stdlib, kernel]},
  {registered, [etalk]},
  {mod, {etalk, []}},
  {env,
   [{max_restart, 3},
    {max_time, 10}
   ]}
 ]}.
