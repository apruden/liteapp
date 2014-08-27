-module(home_handler).

-export([home/1, index/1]).


index(Req) ->
	{ok, PubKey} = application:get_env(genapp, pub_key),
	PubKeyHex = integer_to_list(element(2, PubKey), 16),
	PubExpHex = integer_to_list(element(3, PubKey), 16),
	liteapp_web:reply_view("index", [{mod, PubKeyHex}, {exp, PubExpHex}]).

home(Req) ->
	liteapp_web:reply_view("home", [], Req).
