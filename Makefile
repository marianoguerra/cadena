
.PHONY: devrel
devrel:
	rebar3 as node1 release
	rebar3 as node2 release
	rebar3 as node3 release

node1-console:
	_build/node1/rel/cadena/bin/cadena console

node2-console:
	_build/node2/rel/cadena/bin/cadena console

node3-console:
	_build/node3/rel/cadena/bin/cadena console

