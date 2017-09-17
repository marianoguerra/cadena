cadena
======

Create Project
--------------

rebar3 new app name=cadena
cd cadena

::

	.
	├── LICENSE
	├── README.md
	├── rebar.config
	└── src
		├── cadena_app.erl
		├── cadena.app.src
		└── cadena_sup.erl

	1 directory, 6 files

Configuring Dev Release
-----------------------

1. Add Dependencies
2. Configure relx section

    1. Add overlay variables file vars.config
    2. Add sys.config
    3. Add vm.args

Build a release to test that everything is setup correctly::

	$ rebar3 release

Run the release interactively with a console::

	$ _build/default/rel/cadena/bin/cadena console

Output (edited and paths redacted for clarity)::

	Exec: erlexec
		-boot _build/default/rel/cadena/releases/0.1.0/cadena
		-boot_var ERTS_LIB_DIR erts-8.3/../lib
		-mode embedded
		-config _build/default/rel/cadena/generated.conf/app.1.config
		-args_file _build/default/rel/cadena/generated.conf/vm.1.args
		-vm_args _build/default/rel/cadena/generated.conf/vm.1.args
		-- console

	Root: _build/default/rel/cadena
	Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:64]
			      [kernel-poll:true]

	18:31:12.150 [info] Application lager started on node 'cadena@127.0.0.1'
	18:31:12.151 [info] Application cadena started on node 'cadena@127.0.0.1'
	Eshell V8.3  (abort with ^G)
	(cadena@127.0.0.1)1>

Quit::

	(cadena@127.0.0.1)1> q().
	ok

Non interactive start::

	$ _build/default/rel/cadena/bin/cadena start

No output is generated if it's started, we can check if it's running by pinging
the application::

	$ _build/default/rel/cadena/bin/cadena ping

We should get::

	pong

If we want we can attach a console to the running system::

	$ _build/default/rel/cadena/bin/cadena attach

Output::

	Attaching to /tmp/erl_pipes/cadena@127.0.0.1/erlang.pipe.1 (^D to exit)

	(cadena@127.0.0.1)1>

If we press Ctrl+d we can dettach the console without stopping the system::

	(cadena@127.0.0.1)1> [Quit]

We can stop the system whenever we want issuing the stop command::

	$ _build/default/rel/cadena/bin/cadena stop

Output::

	ok

.. note::

    Note the Ctrl+d to exit, if we write `q().` not only we dettach the
    console but we also stop the system!

Let's try it.

Non interactive start::

	$ _build/default/rel/cadena/bin/cadena start

No output is generated if it's started, we can check if it's running by pinging
the application::

	$ _build/default/rel/cadena/bin/cadena ping

We should get::

	pong

If we want we can attach a console to the running system::

	$ _build/default/rel/cadena/bin/cadena attach

Output::

	Attaching to /tmp/erl_pipes/cadena@127.0.0.1/erlang.pipe.1 (^D to exit)

	(cadena@127.0.0.1)1>

Now let's quit with q()::

	(cadena@127.0.0.1)1> q().

Output::

	ok

Now let's see if it's alive::

	$ _build/default/rel/cadena/bin/cadena ping

	Node 'cadena@127.0.0.1' not responding to pings.

Configure Prod and Dev Cluster Releases
---------------------------------------

Building Prod Release
.....................

We start by adding a new section to rebar.config called profiles, and define
4 profiles that override the default release config with specific values,
let's start by trying the prod profile, which we will use to create production
releases of the project::

    rebar3 as prod release

Output::

    ===> Verifying dependencies...
    ...
    ===> Compiling cadena
    ===> Running cuttlefish schema generator
    ===> Starting relx build process ...
    ===> Resolving OTP Applications from directories:
              _build/prod/lib
              erl-19.3/lib
    ===> Resolved cadena-0.1.0
    ===> Including Erts from erl-19.3
    ===> release successfully created!

Notice now that we have a new folder in the _build directory::

    $ ls -1 _build

Output::

    default
    prod

The results of the commands run "as prod" are stored in the prod folder.

You will notice if you explore the prod/rel/cadena folder that there's a folder
called erts-8.3 (the version may differ if you are using a different erlang
version), that folder is there because of the `include_erts` option we overrided
in the prod profile.

This means you can yip the _build/prod/rel/cadena folder, upload it to a server
that doesn't have erlang install it and you can still run your release there.

This is a good way to be sure that the version running in production is the
same you use in development.

Just be careful with deploying to an operating system too different to the one
you used to create the release becase you may have problems with bindings like
libc or openssl.

Running it is done as usual, only the path changes::

    _build/prod/rel/cadena/bin/cadena console

    _build/prod/rel/cadena/bin/cadena start
    _build/prod/rel/cadena/bin/cadena ping
    _build/prod/rel/cadena/bin/cadena attach
    _build/prod/rel/cadena/bin/cadena stop

Building Dev Cluster Releases
.............................

To build a cluster we need at least 3 nodes, that's why the last 3 profiles
are node1, node2 and node3, they need to have different node names, for that
we use the overlay var files to override the name of each.

Now let's build them::

    rebar3 as node1 release
    rebar3 as node2 release
    rebar3 as node3 release

The output for each should be similar to the one for the prod release.

Now on three different shells start each node::

    ./_build/node1/rel/cadena/bin/cadena console

Check the name of the node in the shell::

    (node1@127.0.0.1)1>

Do the same for node2 and node3 on different shells::

    ./_build/node2/rel/cadena/bin/cadena console
    ./_build/node3/rel/cadena/bin/cadena console
 
You should get respectively::

    (node2@127.0.0.1)1>

And::

    (node3@127.0.0.1)1>

In case you don't remember, you can quit with `q().`

Joining the Cluster Together
----------------------------

Until here we built 3 releases of the same code with slight modifications to
allow running a cluster on one computer, but 3 nodes running doesn't mean
we have a cluster, for that we need to use what we learned in the `Multi-Paxos with riak_ensemble Part 1 <http://marianoguerra.org/posts/multi-paxos-with-riak_ensemble-part-1/>`_ but now on code and not interactively.

For that we will create a cadena_console module that we will use to make calls
from the outside and trigger actions on each node, the code is similar to the
one presented in `Multi-Paxos with riak_ensemble Part 1 <http://marianoguerra.org/posts/multi-paxos-with-riak_ensemble-part-1/>`_.

.. code:: erlang


    join([NodeStr]) ->
        % node name comes as a list string, we need it as an atom
        Node = list_to_atom(NodeStr),
        % check that the node exists and is alive
        case net_adm:ping(Node) of
            % if not, return an error
            pang ->
                {error, not_reachable};
            % if it replies, let's join him passing our node reference
            pong ->
                riak_ensemble_manager:join(Node, node())
        end.

    create([]) ->
        % enable riak_ensemble_manager
        riak_ensemble_manager:enable(),
        % wait until it stabilizes
        wait_stable().

    cluster_status() ->
        case riak_ensemble_manager:enabled() of
            false ->
                {error, not_enabled};
            true ->
                Nodes = lists:sort(riak_ensemble_manager:cluster()),
                io:format("Nodes in cluster: ~p~n",[Nodes]),
                LeaderNode = node(riak_ensemble_manager:get_leader_pid(root)),
                io:format("Leader: ~p~n",[LeaderNode])
        end.

We also need to add the riak_ensemble supervisor to our supervisor tree in cadena_sup:

.. code:: erlang

    init([]) ->
        % get the configuration from sys.config
        DataRoot = application:get_env(riak_ensemble, data_root, "./data"),
        % create a unique path for each node to avoid clashes if running more
        % than one node in the same computer
        NodeDataDir = filename:join(DataRoot, atom_to_list(node())),

        Ensemble = {riak_ensemble_sup,
                    {riak_ensemble_sup, start_link,
                     [NodeDataDir]},
                    permanent, 20000, supervisor, [riak_ensemble_sup]},

        {ok, { {one_for_all, 0, 1}, [Ensemble]} }.


Before building the dev cluster we need to add the `crypto` app to cadena.app.src
since it's needed by riak_ensemble to create the cluster.

Now let's build the dev cluster, I created a Makefile to make it simpler::

    make devrel

Now on three different shells run one command on each::

    make node1-console
    make node2-console
    make node3-console

Now let's make an rpc call to enable the riak_ensemble cluster on node1::

    ./_build/node1/rel/cadena/bin/cadena rpc cadena_console create

On node1 you should see something like::

    [info] {root,'node1@127.0.0.1'}: Leading

Now let's join node2 to node1::

    ./_build/node2/rel/cadena/bin/cadena rpc cadena_console join node1@127.0.0.1

On node1 you should see::

    [info] join(Vsn): {1,152} :: 'node2@127.0.0.1' :: ['node1@127.0.0.1']

On node2::

    [info] JOIN: success

Finally let's join node3::

    ./_build/node3/rel/cadena/bin/cadena rpc cadena_console join node1@127.0.0.1

Output on node1::

    [info] join(Vsn): {1,453} :: 'node3@127.0.0.1' :: ['node1@127.0.0.1','node2@127.0.0.1']


On node3::

    [info] JOIN: success

Let's check that the 3 nodes have the same view of the cluster, let's ask node1
what's the ensemble status::

    ./_build/node1/rel/cadena/bin/cadena rpc cadena_console ensemble_status

::

    Nodes in cluster: ['node1@127.0.0.1','node2@127.0.0.1','node3@127.0.0.1']
    Leader: 'node1@127.0.0.1'

node2::

    $ ./_build/node2/rel/cadena/bin/cadena rpc cadena_console ensemble_status

::

    Nodes in cluster: ['node1@127.0.0.1','node2@127.0.0.1','node3@127.0.0.1']
    Leader: 'node1@127.0.0.1'

node3::

    $ ./_build/node3/rel/cadena/bin/cadena rpc cadena_console ensemble_status

::

    Nodes in cluster: ['node1@127.0.0.1','node2@127.0.0.1','node3@127.0.0.1']
    Leader: 'node1@127.0.0.1'

Everything looks right, stop the 3 nodes (`q().`) and start them again, you
will see that after starting up node1 logs::

    [info] {root,'node1@127.0.0.1'}: Leading

And if you call ensemble_status on any node you get the same outputs as before,
this means they remember the cluster topology even after restarts.

Key/Value Store HTTP API
------------------------

To have an HTTP API we will need an HTTP server, in our case we will use
`Cowboy 2.0 RC 3 <https://ninenines.eu/docs/#cowboy>`_, for that we need to:

1. Add it as a dependency (we will load if from git since it's still a release candidata)
2. Add it to our list of applications to start when our application starts
3. Add it to the list of dependencies to include in our release
4. Set up the HTTP listener and routes when our application starts
   + Read the cowboy documentation for details on how to `set up the listener <https://ninenines.eu/docs/en/cowboy/2.0/guide/listeners/>`_

We setup just one route that is handled by the cadena_h_keys module, it's a
plain HTTP handler, no fancy REST stuff for now, there we handle the request on
the init/2 function itself, we pattern match against the method field on the
request object and handle:

POST
	set a key in a given ensemble to the value sent in the JSON request body

GET
	get a key in a given ensemble, if not found null will be returned in the value field in the response

DELETE
	delete a key in a given ensemble, returns null both if the key existed and if itdidn't

Any other method would get a 405 Method Not Allowed response.

The route has the format /keys/<ensemble>/<key>, for now we only allow the root
ensemble to be set in the <ensemble> part of the path.

We also add the jsone library to encode/decode JSON and the lager library to
log messages.

We add both to the list of dependencies to include in the release, we don't add
them to the list of applications in app.src since we don't need to start
anything for them, they are just libraries we use.

We will also need to have a way to override the HTTP port where each instance
listens to so we can run a cluster on one computer and each node can listen
for HTTP requests on a different port.

The dev and prod releases will listen on 8080 as specified in vars.config.

node1 will listen on port 8081 (override in vars_node1.config)

node2 will listen on port 8082 (override in vars_node2.config)

node3 will listen on port 8083 (override in vars_node3.config)

To avoid having to configure this in sys.config we will define a cuttlefish
schema in config.schema that cuttlefish will use to generate a default config
file and validation code for us.

We have to replace the variables from variable overrides in our config.schema
file for each release before it's processed by cuttlefish itself, for that we
use the template directive on an overlay section on the release config.

Build devrel:

.. code:: sh

	make revrel

Check the configuration file generated for each node at::

	_build/node1/rel/cadena/etc/cadena.conf
	_build/node2/rel/cadena/etc/cadena.conf
	_build/node3/rel/cadena/etc/cadena.conf

The first part is of interest to use, it looks like this for node1::


	## port to listen to for HTTP API
	## 
	## Default: 8081
	## 
	## Acceptable values:
	##   - an integer
	http.port = 8081

	## number of acceptors to user for HTTP API
	## 
	## Default: 100
	## 
	## Acceptable values:
	##   - an integer
	http.acceptors = 100

	## folder where ensemble data is stored
	## 
	## Default: ./cadena_data
	## 
	## Acceptable values:
	##   - text
	data.dir = ./cadena_data


Start 3 nodes in 3 different shells:

.. code:: sh

	make node1-console
	make node2-console
	make node3-console

Start enseble and join nodes, I created a target on the Makefile to make it easier:

.. code:: sh

	make devrel-setup

Let's set **key1** in ensemble **root** to **42** on node1 (port 8081):

.. code:: sh

    curl -X POST http://localhost:8081/keys/root/key1 -d 42

Response:

.. code:: js

    {"data":{"epoch":2,"key":"key1","seq":10,"value":42},"ok":true}

Let's get **key1** in ensemble **root** to **42** on node2 (port 8082):

.. code:: sh

    curl -X GET http://localhost:8082/keys/root/key1

Response:

.. code:: js

    {"data":{"epoch":2,"key":"key1","seq":10,"value":42},"ok":true}

Same on node3:

.. code:: sh

    curl -X GET http://localhost:8083/keys/root/key1

Response:

.. code:: js

    {"data":{"epoch":2,"key":"key1","seq":10,"value":42},"ok":true}

Overwrite on node1:

.. code:: sh

    curl -X POST http://localhost:8081/keys/root/key1 -d '{"number": 42}'

Response:

.. code:: js

    {"data":{"epoch":2,"key":"key1","seq":400,"value":{"number":42}},"ok":true}

Get on node2:

.. code:: sh

	curl -X GET http://localhost:8082/keys/root/key2

.. code:: js

	{"data":{"epoch":3,"key":"key2","seq":11,"value":null},"ok":true}

Let's set **key2** in ensemble **root** to **{"number": 42}** on node1 (port 8082):

.. code:: sh

	curl -X POST http://localhost:8081/keys/root/key2 -d '{"number": 42}'

Response:

.. code:: js

	{"data":{"epoch":3,"key":"key2","seq":67,"value":{"number":42}},"ok":true}

Get it on node2:

.. code:: sh

	curl -X GET http://localhost:8082/keys/root/key2

Response:

.. code:: js

	{"data":{"epoch":3,"key":"key2","seq":67,"value":{"number":42}},"ok":true}

Delete **key2** in ensemble **root** on node2:

.. code:: sh

	curl -X DELETE http://localhost:8082/keys/root/key2

Response:

.. code:: js

	{"data":{"epoch":3,"key":"key2","seq":137,"value":null},"ok":true}

Check that it was removed by trying to get it again on node2:

.. code:: sh

	curl -X GET http://localhost:8082/keys/root/key2

Response:

.. code:: js

	{"data":{"epoch":3,"key":"key2","seq":137,"value":null},"ok":true}
