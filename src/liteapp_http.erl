-module(liteapp_http).

-export([parse_request/1,
		status/1,
		split_path/1,
		encode_headers/1,
		split_args/1,
		get_body/3]).

-include("liteapp.hrl").

parse_request(Buf) ->
	parse_method(Buf, #req{}).

get_body(Socket, Buf, Req) when byte_size(Buf) < Req#req.content_length ->
	case gen_tcp:recv(Socket) of
		{ok, Buf0} ->
			Diff = byte_size(Buf) + byte_size(Buf0) - Req#req.content_length,
			case Diff > 0 of
				true ->
					Diff0 = Req#req.content_length - byte_size(Buf),
					<<D1:Diff0/binary, D2/binary>> = Buf0,
					Req#req{body = <<Buf/binary, D1/binary>>, next_buff = binary:copy(D2)};
				false ->
					get_body(Socket, <<Buf/binary, Buf0/binary>>, Req)
			end;
		{error, _Reason} ->
			%send_response(Socket, Req, {400, [], <<"Invalid">>})
			invalid
	end;
get_body(_Socket, Buf, Req) ->
	Req#req {body = Buf}.

parse_method(<<"GET ", Rest/binary>>, Req) ->
	parse_path(Rest, Req#req {method = 'GET'});
parse_method(<<"POST ", Rest/binary>>, Req) ->
	parse_path(Rest, Req#req {method = 'POST'});
parse_method(_, Req) ->
	Req.

parse_path(<<" ", Rest/binary>>, Req) ->
	parse_version(Rest, Req);
parse_path(<<A:1/binary, Rest/binary>>, #req {path = undefined} = Req) ->
	parse_path(Rest, Req#req {path = A});
parse_path(<<A:1/binary, Rest/binary>>, #req {path = Uri} = Req) ->
	parse_path(Rest, Req#req {path = <<Uri/binary, A/binary>>}).

parse_version(<<"HTTP/1.1\r\n", Rest/binary>>, Req) ->
	parse_headers(Rest, Req#req {http_vsn = {1, 1}});
parse_version(<<"HTTP/1.0\r\n", Rest/binary>>, Req) ->
	parse_headers(Rest, Req#req {http_vsn = {1, 0}});
parse_version(_, Req) ->
	Req.

parse_headers(<<"\r\n\r\n", Rest/binary>>, Req) ->
	{Req, Rest};
parse_headers(<<"Connection: close", Rest/binary>>, Req) ->
	parse_headers(Rest, Req#req {connection = close});
parse_headers(<<"Connection: keep-alive", Rest/binary>>, Req) ->
	parse_headers(Rest, Req#req {connection = keep_alive});
parse_headers(<<"Content-Length: ", Rest/binary>>, Req) ->
	parse_content_length(Rest, <<"">>, Req);
parse_headers(<<"Authorization: ", Auth:43/binary, Rest/binary>>, Req) ->
	parse_headers(Rest, Req#req {auth = Auth});
parse_headers(<<_:1/binary, Rest/binary>>, Req) ->
	parse_headers(Rest, Req).

parse_content_length(<<D:1/binary, _Rest/binary>> = Buf, Len, Req) when D =:= <<"\r">> ->
	parse_headers(Buf, Req#req{content_length = binary_to_integer(Len)});
parse_content_length(<<D:1/binary, Rest/binary>>, Len, Req) ->
	parse_content_length(Rest, <<Len/binary, D/binary>>, Req).

encode_headers([]) ->
	[];
encode_headers([[] | H]) ->
	encode_headers(H);
encode_headers([{K, V} | H]) ->
	[encode_value(K), <<": ">>, encode_value(V), <<"\r\n">>, encode_headers(H)].

encode_value(V) when is_integer(V) -> integer_to_binary(V);
encode_value(V) when is_binary(V)  -> V;
encode_value(V) when is_list(V) -> list_to_binary(V).

split_path(Path) ->
	[P || P <- binary:split(Path, [<<"/">>], [global]), P =/= <<>>].

split_args(<<"">>) ->
	[];
split_args(Qs) ->
	Tokens = binary:split(Qs, <<"&">>, [global, trim]),
	[case binary:split(Token, <<"=">>) of
			[Token] -> {Token, true};
			[Name, Value] -> {Name, Value}
		end || Token <- Tokens].


status(Code) ->
	case Code of
		200 -> <<"200 OK">>;
		201 -> <<"201 OK">>;
		400 -> <<"400 Bad request">>;
		401 -> <<"401 Unauthorized">>;
		404 -> <<"404 Not found">>;
		409 -> <<"409 Conflict">>;
		500 -> <<"500 Server error">>;
		503 -> <<"503 Server unavailable">>
	end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_request_test() ->
	{Req, _Buf}= parse_request(<<"GET / HTTP/1.1\r\nDate: 123\r\nConnection: keep-alive\r\nAuthorization: basic\r\nContent-Length: 123456789\r\n\r\ntoto">>),
	?assert('GET' =:= Req#req.method),
	?assert(<<"/">> =:= Req#req.path),
	?assert(keep_alive =:= Req#req.connection),
	?assert(123456789 =:= Req#req.content_length),
	ok.

split_path_test() ->
	Res = split_path(<<"/">>),
	Res1 = split_path(<<"/user/login/toto">>),
	?assert([] =:= Res),
	?assert([<<"user">>, <<"login">>, <<"toto">>] =:= Res1),
	ok.

-endif.
