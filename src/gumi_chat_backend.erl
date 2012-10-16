%% Feel free to use, reuse and abuse the code in this file.

-module(gumi_chat_backend).
-behaviour(application).
-export([start/0, start/2, stop/1]).

start() ->
	application:start(crypto),
	application:start(public_key),
	application:start(ssl),
	application:start(cowboy),
	application:start(gumi_chat_backend).

start(_Type, _Args) ->
	gumi_chat_msg_router:start_link(),
	Dispatch = [
		{'_', [
			{[<<"msg_channel">>], websocket_handler, []},
			{[<<"eventsource">>], eventsource_handler, []},
			{[<<"eventsource">>, <<"live">>], eventsource_emitter, []},
			{'_', default_handler, []}
		]}
	],
	cowboy:start_listener(my_http_listener, 100,
		cowboy_tcp_transport, [{port, 8001}],
		cowboy_http_protocol, [{dispatch, Dispatch}]
	),
	%cowboy:start_listener(my_https_listener, 100,
	%	cowboy_ssl_transport, [
	%		{port, 8443}, {certfile, "priv/ssl/cert.pem"},
	%		{keyfile, "priv/ssl/key.pem"}, {password, "cowboy"}],
	%	cowboy_http_protocol, [{dispatch, Dispatch}]
	%),
	gumi_chat_backend_sup:start_link().

stop(_State) ->
	ok.
	