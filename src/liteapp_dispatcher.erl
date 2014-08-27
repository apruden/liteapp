-module(liteapp_dispatcher).

-export([handle/1]).

-include("liteapp.hrl").
-include_lib("kernel/include/file.hrl").

-define(COMPRESS_THRESHOLD, 1024).

handle(Req) ->
	case handle(Req#req.method, liteapp_http:split_path(Req#req.path), Req) of
		{Code, Headers, Body} ->
			compress_if_needed(Code, Headers, Body);
		Response ->
			Response
	end.

handle('GET', [<<"app">> | FilePath], Req) ->
	io:format("~w~n", [FilePath]),
	Filename = local_path(FilePath),
	case file_size(Filename) of
		{error, _Reason} ->
			{404, [], <<"Not found">>};
		{ok, Size} ->
			{200, headers(Filename, Size), {file, Filename, 0, Size}}
	end;
handle('GET', [], Req) ->
%	{200, [], <<"Hello World!">>};
	home_handler:index(Req);
handle(_, _, _Req) ->
	{404, [], <<"Not Found">>}.

handle('GET',[<<"ping">>], _Req, _Sess) ->
	{200, [], <<"Hello World!">>}.

compress_if_needed(ResponseCode, Headers, Body) ->
	case should_compress(Body) of
		false ->
			{ResponseCode, Headers, Body};
		true ->
			{CompressedBody, Encoding} = compress(Body),
			{ResponseCode, [{<<"Content-Encoding">>, Encoding} | Headers], CompressedBody}
	end.

compress(Body) ->
	{zlib:gzip(Body), <<"gzip">>}.

should_compress(Body) ->
	is_binary(Body) andalso byte_size(Body) >= ?COMPRESS_THRESHOLD orelse
	is_list(Body) andalso iolist_size(Body) >= ?COMPRESS_THRESHOLD.

local_path(FilePath) ->
	MappedPath = <<"/home/alex/workspace_erlang/liteapp/www/">>,
	filename:join(filename:flatten([MappedPath, FilePath])).

file_size(undefined) ->
	{error, illegal_path};

file_size(Filename) ->
	case file:read_file_info(Filename, [{time, posix}]) of
		{ok, #file_info{type = regular, access = Perm, size = Size}} when Perm =:= read orelse Perm =:= read_write ->
			{ok, Size};
		{error, Reason} ->
			{error, Reason};
		_ -> 
			{error, invalid_file}
	end.

headers(Filename, Size) ->
	case mime_type(Filename) of
		undefined ->
			[{"Content-Length", Size}];
		MimeType ->
			[{"Content-Length", Size}, {"Content-Type", MimeType}]
	end.

mime_type(Filename) when is_binary(Filename) ->
	case filename:extension(Filename) of
		<<>> ->
			undefined;
		<<$., Ext/binary>> ->
			case Ext of
				<<"js">> -> <<"text/javascript">>;
				<<"css">> -> <<"text/css">>;
				<<"jpg">> -> <<"image/jpeg">>;
				<<"png">> -> <<"image/png">>;
				_ -> undefined
			end
	end.
