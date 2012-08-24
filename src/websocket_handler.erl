%% Feel free to use, reuse and abuse the code in this file.

-module(websocket_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_http_websocket_handler).
-export([init/3, handle/2, terminate/2]).
-export([websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

init({_Any, http}, Req, []) ->
	case cowboy_http_req:header('Upgrade', Req) of
		{undefined, Req2} -> {ok, Req2, undefined};
		{<<"websocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket};
		{<<"WebSocket">>, _Req2} -> {upgrade, protocol, cowboy_http_websocket}
	end.

handle(Req, State) ->

	{User_id, _Req1}=cowboy_http_req:qs_val(<<"u">>, Req, <<"0">>),

	Reply=if User_id==<<"0">> -> <<"<html>
<body>
malformed url!
</body>
</html>">>;
			 true -><<"<html>
<head>
<script type=\"text/javascript\">
function addStatus(text){
	var date = new Date();
	document.getElementById('status').innerHTML
		= document.getElementById('status').innerHTML
		+ date + \": \" + text + \"<br/>\";
}
function ready(){
	if (\"MozWebSocket\" in window) {
		WebSocket = MozWebSocket;
	}
	if (\"WebSocket\" in window) {
		// browser supports websockets
		alert(\"ws://localhost:8080/msg_channel/?u=",User_id/binary,"\");
		var ws = new WebSocket(\"ws://localhost:8080/msg_channel/?u=",User_id/binary,"\");
		ws.onopen = function() {
			// websocket is connected
			addStatus(\"websocket connected!\");
			// send hello data to server.
			ws.send(\"hello server!\");
			addStatus(\"sent message to server: 'hello server'!\");
		};
		ws.onmessage = function (evt) {
			var receivedMsg = evt.data;
			addStatus(\"server sent the following: '\" + receivedMsg + \"'\");
		};
		ws.onclose = function() {
			// websocket was closed
			addStatus(\"websocket was closed\");
		};

		/*
		window.setTimeout(function(){
			window.setInterval(function(){
				var msg='action=send_msg&user_id_to=",User_id/binary,"&msg=Msg_from_self';
				ws.send(msg);
			},2000);
		},2000);
		*/

		window.setTimeout(function(){
			window.setInterval(function(){
				var msg='action=broadcast_msg&msg=Broadcast_msg_from_self';
				ws.send(msg);
			},3000);
		},3000);

	} else {
		// browser does not support websockets
		addStatus(\"sorry, your browser does not support websockets.\");
	}
}
</script>
</head>
<body onload=\"ready();\">
Hi!
<div id=\"status\"></div>
</body>
</html>">>
	end,
	
	{ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"text/html">>}],Reply, Req),
	{ok, Req2, State}.


terminate(_Req, _State) ->
	ok.

get_user_id(Req)->
	case cowboy_http_req:qs_val(<<"u">>, Req, undefined) of
		{<<"">>,_Req1} -> 
			undefined;
		{User_id,_Req1} -> 
			User_id
	end.

websocket_init(_Any, Req, []) ->
	%timer:send_interval(1000, tick),
	%timer:send_interval(1500, {text, <<"Msg">>}),
	%timer:send_interval(3000, {text, <<"Msg">>, get_user_id(Req)}),
	%timer:send_interval(2000, {broadcast, <<"Msg from broadcast">>}),
	%Req2 = cowboy_http_req:compact(Req),
	
	case get_user_id(Req) of
		undefined -> 
			io:format('websocket_init(_Any, Req, []); 0\n',[]),
			{shutdown, Req};
		User_id -> 
			io:format('websocket_init(_Any, Req, []); ~p\n',[User_id]),
			gumi_chat_msg_router:login(User_id, self()),
			{ok, Req, undefined, hibernate}
	end.


websocket_handle({text, Msg}, Req, State) ->
	Query_args=mochiweb_util:parse_qs(Msg),
	Action=mochiweb_util:extract_query_arg_value_as_str(Query_args,"action","(empty)"),
	io:format('websocket_handle({text, Msg}, Req, State) ~p ~p ~p ~p\n',[Msg,Query_args,"action",Action]),
	
	case Action of
		"send_msg"->
			Msg1 = list_to_binary(mochiweb_util:extract_query_arg_value_as_str(Query_args,"msg","(empty)")), 
			User_id_to = list_to_binary(mochiweb_util:extract_query_arg_value_as_str(Query_args,"user_id_to","0")), 
			gumi_chat_msg_router:send(Msg1, get_user_id(Req), User_id_to),
			{reply, {text, << "You sent '", Msg1/binary,"' to ",User_id_to/binary >>}, Req, State, hibernate};
		
		"broadcast_msg"->
			Msg1 = list_to_binary(mochiweb_util:extract_query_arg_value_as_str(Query_args,"msg","(empty)")), 
			gumi_chat_msg_router:broadcast(Msg1, get_user_id(Req)),
			{reply, {text, << "You broadcast '", Msg1/binary >>}, Req, State, hibernate};
		
		_ ->
			{reply, {text, << "You said: ", Msg/binary>>}, Req, State, hibernate}
	end;

websocket_handle(_Any, Req, State) ->
	io:format('handle unknown\n',[]),
	{ok, Req, State}.

websocket_info(tick, Req, State) ->
	io:format('websocket_info(tick, Req, State)\n',[]),
	%{User_id, _Req1}=cowboy_http_req:qs_val(<<"u">>, Req, <<"0">>),
	%gumi_chat_msg_router:send(User_id, <<"msg from router.">>),
	{reply, {text, <<"Tick">>}, Req, State, hibernate};

%websocket_info({boardcast, _Msg}, Req, State) ->
%	%{User_id, _Req1}=cowboy_http_req:qs_val(<<"u">>, Req, <<"0">>),
%	io:format('websocket_info({boardcast, Msg}, Req, State)\n',[]),
%	%gumi_chat_msg_router:broadcast(Msg),
%	{ok, Req, State, hibernate};

websocket_info({text, Msg}, Req, State) ->
	io:format('websocket_info({text, Msg}, Req, State) \n',[]),
	Msg1= <<"[src:(unknown)] ",Msg/binary>>,
	{reply, {text, Msg1}, Req, State, hibernate};

websocket_info({text, Msg, User_id}, Req, State) ->
	io:format('websocket_info({text, Msg, User_id}, Req, State) \n',[]),
	Msg1= <<"[src:",User_id/binary,"] ", Msg/binary>>,
	{reply, {text, Msg1}, Req, State, hibernate};

websocket_info(_Info, Req, State) ->
	io:format('websocket_info(_Info, Req, State) \n',[]),
	{ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
	io:format('websocket_terminate(_Reason, _Req, _State) \n',[]),
	gumi_chat_msg_router:logout(self()),
	ok.
