#!/bin/sh

clear;
make;

erl -sname gumi_chat_backend -pa ebin -pa deps/*/ebin -s gumi_chat_backend \
	-eval "io:format(\"~n~nThe following examples are available:~n\")." \
	-eval "io:format(\"* Hello world: http://localhost:8001~n\")." \
	-eval "io:format(\"* Websockets: http://localhost:8001/msg_channel/?u=10000~n\")." \
	-eval "io:format(\"* Eventsource: http://localhost:8001/eventsource~n\")." \
	+K true
