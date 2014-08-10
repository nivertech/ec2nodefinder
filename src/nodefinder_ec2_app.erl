%%------------------------------------------------------------------------------
%% @doc ec2-describe-instances based nodefinder service.
%% @end
%%------------------------------------------------------------------------------
-module(nodefinder_ec2_app).
-behaviour (application).

-export([
    discover/0,
    first_security_group/0,
    first_keypair/0,
    start/0,
    start/2,
    stop/0,
    stop/1
]).

-define(APPLICATION, nodefinder_ec2).

%%------------------------------------------------------------------------------
%%                                Public
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Initiate a discovery request.  Discovery is synchronous; the
%% results are returned.
%% @end
%%------------------------------------------------------------------------------
-spec discover() -> {ok, [{atom(), pong|pang|timeout}]}.
discover() ->
    nodefinder_ec2_srv:discover().

%%------------------------------------------------------------------------------
%%                        application callbacks                        
%%------------------------------------------------------------------------------

%% @hidden
start() ->
    inets:start(),
    crypto:start(),
    application:start(?APPLICATION).

%% @hidden
start(_Type, _Args) ->
    Group = case application:get_env(?APPLICATION, group) of
                {ok, G} -> 
                    G;
                _       -> 
                    first_security_group()
            end,
    Keypair = case application:get_env(?APPLICATION, keypair) of
                {ok, K} -> 
                    K;
                _       -> 
                    first_keypair()
            end,
    {ok, PingTimeoutSec} = application:get_env(?APPLICATION, ping_timeout_sec),
    AKI =   case get_p(access, "AWS_ACCESS_KEY") of
                false -> 
                    get_p(access, "AWS_ACCESS_KEY_ID");
                AccessKey ->
                    AccessKey
            end,
    SAK =   case get_p(secret, "AWS_SECRET_KEY") of
                false ->
                    get_p(secret, "AWS_SECRET_ACCESS_KEY");
                SecretKey ->
                    SecretKey
            end,

    nodefinder_ec2_sup:start_link(  Group,
                                    Keypair,
                                    PingTimeoutSec*1000,
                                    AKI,
                                    SAK).

%% @hidden
stop () -> 
  application:stop(?APPLICATION).

%% @hidden
stop (_State) ->
  ok.

%%------------------------------------------------------------------------------
%%                               Private                               
%%------------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc get application parameter from .app file, 
%%      if not exist check environment variable
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec get_p(Atom::atom(), Env::string())-> error|term().
get_p(Atom, Env)->
    case application:get_env(?APPLICATION, Atom) of
        {ok, Value} ->
            Value;
        undefined ->
            case os:getenv(Env) of
                false -> error;
                Value -> Value
            end
    end.

%%------------------------------------------------------------------------------
%% @doc get first EC2 security group
%% @end
%%------------------------------------------------------------------------------
-spec first_security_group() -> string().    
first_security_group() ->
    Url = "http://169.254.169.254/2007-08-29/meta-data/security-groups",
    case httpc:request(Url) of
        {ok, {{_HttpVersion, 200, _Reason}, _Headers, Body}} ->
            string:substr(Body, 1, string:cspan(Body, "\n"));
        _BadResult ->
            %erlang:error({ http_request_failed, Url, BadResult })
            "default"
    end.

%%------------------------------------------------------------------------------
%% @doc get first EC2 keypair
%% @end
%%------------------------------------------------------------------------------
% curl -s http://169.254.169.254/latest/meta-data/public-keys/
% 0=kl-nivertech
-spec first_keypair() -> string().    
first_keypair() ->
    Url = "http://169.254.169.254/latest/meta-data/public-keys/",
    case httpc:request(Url) of
        {ok, {{_HttpVersion, 200, _Reason}, _Headers, Body}} ->
            % TODO - support multiple keys
            string:sub_string(Body, string:chr(Body, $=)+1, string:cspan(Body, "\n"));
        _BadResult ->
            %erlang:error({ http_request_failed, Url, BadResult })
            "default"
    end.
