
.PHONY: devrel
devrel:
	rebar3 as node1 release
	rebar3 as node2 release
	rebar3 as node3 release

devrel-setup:
	_build/node1/rel/cadena/bin/cadena rpc cadena_console create
	_build/node2/rel/cadena/bin/cadena rpc cadena_console join node1@127.0.0.1
	_build/node3/rel/cadena/bin/cadena rpc cadena_console join node1@127.0.0.1
	_build/node1/rel/cadena/bin/cadena rpc cadena_console ensemble_status

node1-console:
	_build/node1/rel/cadena/bin/cadena console

node2-console:
	_build/node2/rel/cadena/bin/cadena console

node3-console:
	_build/node3/rel/cadena/bin/cadena console

