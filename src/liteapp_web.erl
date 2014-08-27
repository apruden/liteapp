-module(liteapp_web).

-export([reply_data/1,
		reply_data/2,
		reply_view/2,
		reply_redirect/1,
		is_authenticated/1,
		check_permission/3,
		check_authorized/1
		]).


reply_redirect(Url) ->
	{302, [{<<"Location">>, Url}], <<"">>}.


reply_data(Status, Term) ->
	{Status, [{<<"Content-Type">>, <<"application/octet-stream">>}], genapp_util:dumps(Term)}.


reply_data(Term) ->
	reply_data(200, Term).


reply_view(Name, ViewData) ->
	{ok, Root} = application:get_env(liteapp, root),
	%TemplateName = list_to_atom(Name ++ "_dtl"),
	%erlydtl:compile(Root ++ "/" ++ Name ++ ".html", TemplateName),
	%{_, Body} = apply(TemplateName, render, [ViewData]),
	{200, [], <<"hello view!">>}.


%% Implement if HMAC is implemented
%check_auth(Fun, Req, State) ->
%    {Auth, _} = cowboy_req:header(<<"Authorization">>, Req, undefined),
%    case Auth of
%        undefined ->
%            {ok, Res} = cowboy_req:reply(401, [], [], Req),
%            {ok, Res, State};
%        _ ->
%            <<Uid:9/binary,AuthHash/binary>> = Auth,
%            <<_:1/binary,Hash/binary>> = AuthHash,
%            Val = <<"">>, %TODO: formatted post data or query string
%            case validate_hash(Uid, Val, Hash) of
%                true ->
%                    Fun(Req, State);
%                _ ->
%                    {ok, Res} = cowboy_req:reply(401, [], [], Req),
%                    {ok, Res, State}
%           end
%    end.


get_token(Uid, Status, Nick, Req) ->
	Value = <<Status:1/binary, ":", Uid:10/binary>>,
	Hash = base64:encode(crypto:hmac(sha, <<"Admin123">>, Value)),
	<<Value/binary, ":", Hash:28/binary>>.


is_authenticated(Auth) ->
	case Auth of
		undefined ->
			false;
		<<"">> ->
			false;
		_ ->
			<<Value:12/binary, ":", Hash:28/binary, ":", Nick/binary>> = Auth,
			<<Status:1/binary, ":", Uid:10/binary>> = Value,
			{validate_hash(Value, Hash), {Uid, Status, Nick}}
	end.


check_authorized(Auth) ->
	is_authenticated(Auth).


check_permission(Fun, Req, State) ->
	case State of
		{true, <<"0">>, _, _} ->
			genapp_web:reply_redirect("/activate", Req, State);
		{true, <<"1">>, _, _} ->
			genapp_web:reply_redirect("/profiles/me", Req, State);
		{true, <<"2">>, _, _} ->
			Fun(Req,State)
	end.

validate_hash(Val, Hash) ->
	case base64:encode(crypto:hmac(sha, <<"Admin123">>, Val)) of
		Hash ->
			true;
		_ ->
			false
	end.
