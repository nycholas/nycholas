{application, echo,
 [{vsn, "1.0.0"},
  {description, "Simple example application distributed"},
  {modules, [echo, echo_sup, echo_serv]},
  {applications, [stdlib, kernel, crypto]},
  {registered, [echo, echo_sup, echo_serv]},
  {mod, {echo, []}},
  {env,
   [{max_restart, 3},
    {max_time, 10},
    {answers, {<<"Yes">>, <<"No">>, <<"Doubtful">>,
			   <<"I don't like your tone">>, <<"Of course">>,
			   <<"Of course not">>, <<"*backs away slowly and runs away*">>}}
   ]}
 ]}.
