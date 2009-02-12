%% @doc ec2-describe-instances based nodefinder service.
%% @end

-module (ec2nodefinder).
-export ([ discover/0 ]).
-behaviour (application).
-export ([ start/0, start/2, stop/0, stop/1 ]).


%-=====================================================================-
%-                                Public                               -
%-=====================================================================-

%% @spec discover () -> { ok, [ { Node::node (), pong | pang | timeout } ] }
%% @doc Initiate a discovery request.  Discovery is synchronous; the
%% results are returned.
%% @end

discover () ->
  ec2nodefindersrv:discover ().

%-=====================================================================-
%-                        application callbacks                        -
%-=====================================================================-

%% @hidden

start () ->
  application:start (ec2nodefinder).

%% @hidden

start (_Type, _Args) ->
  Group = case application:get_env (ec2nodefinder, group) of
    { ok, G } -> G;
    _ -> first_security_group ()
  end,
  { ok, PingTimeout } = application:get_env (ec2nodefinder, ping_timeout_sec),
  { ok, Access } = application:get_env (ec2nodefinder, access),
  { ok, Secret } = application:get_env (ec2nodefinder, secret),
 
  ec2nodefindersup:start_link (Group, 
                               1000 * PingTimeout,
                               Access,
                               Secret).

%% @hidden

stop () -> 
  application:stop (ec2nodefinder).

%% @hidden

stop (_State) ->
  ok.

%-=====================================================================-
%-                               Private                               -
%-=====================================================================-

%% @private

first_security_group () ->
  Url = "http://169.254.169.254/2007-08-29/meta-data/security-groups",
  case http:request (Url) of
    { ok, { { _HttpVersion, 200, _Reason }, _Headers, Body } } ->
      string:substr (Body, 1, string:cspan (Body, "\n"));
    BadResult ->
      erlang:error ({ http_request_failed, Url, BadResult })
  end.