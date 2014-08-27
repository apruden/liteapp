-module(liteapp_server).

-export([start_link/0, acceptor/2, listener/1, handle_request/2]).

-include("liteapp.hrl").

-define(RECV_TIMEOUT, 2000).

start_link() ->
	{ok, LSocket} = gen_tcp:listen(8999, [{active, false}, binary, {backlog, 1024}]),
	[spawn(?MODULE, listener, [LSocket]) || _ <- lists:seq(1, 4)],
	{ok, self()}.

listener(LSocket)->
	self() ! {ack, self()},
	listener_loop(LSocket).

listener_loop(LSocket) ->
	Pid = self(),
	receive
		{ack, Pid} ->
			spawn(?MODULE, acceptor, [LSocket, Pid]),
			listener_loop(LSocket)
	end.

acceptor(LSocket, Pid) ->
	case gen_tcp:accept(LSocket) of
		{ok, Socket} ->
			Pid ! {ack, Pid},
			handle_request(Socket, <<>>);
		{error, Reason} ->
			exit(Reason)
	end.

%using erlang:decode_packet ~8000 req/s, using erlang parser: ~7300 req/s
%but under realistic load erlang version should outperform BIF.
handle_request(Socket, CurrBuff) ->
	case gen_tcp:recv(Socket, 0, ?RECV_TIMEOUT) of
		{ok, Buf0} ->
			%io:format("~s~n",[Packet]),
			%io:format("~w handling request~n", [self()]),
			{Req, _Buf1} = liteapp_http:parse_request(<<CurrBuff/binary, Buf0/binary>>),
			Resp = liteapp_dispatcher:handle(Req),
			handle_response(Socket, Req, Resp);
		{error, Reason} ->
			%io:format("~w closing error ~w~n", [self(), Reason]),
			gen_tcp:close(Socket),
			exit(Reason)
	end.

handle_response(Socket, Req, {Code, Headers, RespBody}) ->
	RespHeaders = [<<"HTTP/1.1 ">>, liteapp_http:status(Code), <<"\r\n">>, liteapp_http:encode_headers(Headers), <<"\r\n">>],
	case send_response(Socket, Req, RespHeaders, RespBody) of
		ok ->
			case Req#req.connection of
				close ->
					%io:format("~w closing~n", [self()]),
					gen_tcp:close(Socket);
				keep_alive ->
					%io:format("~w ka spawning~n", [self()]),
					Pid = spawn(?MODULE, handle_request, [Socket, Req#req.next_buff]), %spawn new process to force binaries garbage collection
					ok = gen_tcp:controlling_process(Socket, Pid)
			end;
		{error, _Reason} ->
			gen_tcp:close(Socket)
	end,
	exit(normal).

send_response(Socket, _Req, ResponseHeaders, {file, Filename, Offset, Length}) ->
	case file:open(Filename, [read, raw, binary]) of
		{ok, Fd} ->
			try gen_tcp:send(Socket, ResponseHeaders) of
				ok ->
					case file:sendfile(Fd, Socket, Offset, Length, []) of
						{ok, _BytesSent} ->
							ok;
						{error, Reason} when Reason =:= closed orelse Reason =:= enotconn ->
							{error, Reason}
					end;
				{error, Reason} when Reason =:= closed orelse Reason =:= enotconn ->
					{error, Reason}
			after
				file:close(Fd)
			end;
		{error, Reason} ->
			{error, Reason}
	end;
send_response(Socket, _Req, ResponseHeaders, Body) ->
	%<<"HTTP/1.1 200 OK\r\nConnection: keep-alive\r\nContent-Length: 12\r\n\r\nHello World!">>
	case gen_tcp:send(Socket, [ResponseHeaders, Body]) of
		ok ->
			ok;
		{error, Reason} ->
			{error, Reason}
	end.
