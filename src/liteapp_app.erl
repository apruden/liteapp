-module(liteapp_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).


start() ->
	application:ensure_all_started(liteapp).


start(_StartType, _StartArgs) ->
	{ok, Root} = application:get_env(liteapp, root),
	PrivKey = read_pem(Root, priv),
	PubKey = read_pem(Root, pub),
	ok = application:set_env(genapp, priv_key, public_key:pem_entry_decode(PrivKey)),
	ok = application:set_env(genapp, pub_key, public_key:pem_entry_decode(PubKey)),
    liteapp_sup:start_link().

stop(_State) ->
    ok.

read_pem(Root, Type) ->
	{ok, PemBin} = file:read_file(Root ++ "/" ++ atom_to_list(Type) ++ ".pem"),
	[PemEntry] = public_key:pem_decode(PemBin),
	PemEntry.
