%% @hidden

-module (nodefinder_ec2_sup).

-behaviour(supervisor).

-export([ start_link/4, init/1 ]).

%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

start_link (Group, PingTimeout, Access, Secret) ->
  supervisor:start_link(?MODULE, 
     [ Group, PingTimeout, Access, Secret ]).

init ([ Group, PingTimeout, Access, Secret ]) ->

	Server = nodefinder_ec2_srv,
  { ok,
    { { one_for_one, 3, 10 },
      [ { Server,
          { Server, 
            start_link,
            [ Group, PingTimeout, Access, Secret ] },
          permanent,
          10000,
          worker,
          [ Server ]
        }
      ]
    }
  }.

