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

1) Add Dependencies
2) Configure relx section
2.1) Add overlay variables file vars.config
2.2) Add sys.config
2.3) Add vm.args

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

