

#Module exodm_plugin#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


API functions for Exosense server plugin development.

<a name="description"></a>

##Description##


This module contains support functions for Exosense server (exodm) plugins.

<a name="types"></a>

##Data Types##




###<a name="type-account">account()</a>##



<pre>account() = binary()</pre>



###<a name="type-client_key">client_key()</a>##



<pre>client_key() = <a href="#type-key">key()</a></pre>



###<a name="type-config_set">config_set()</a>##



<pre>config_set() = binary()</pre>



###<a name="type-device_id">device_id()</a>##



<pre>device_id() = binary()</pre>



###<a name="type-key">key()</a>##



<pre>key() = binary()</pre>



###<a name="type-key_pair">key_pair()</a>##



<pre>key_pair() = {<a href="#type-client_key">client_key()</a>, <a href="#type-server_key">server_key()</a>}</pre>



###<a name="type-latitude">latitude()</a>##



<pre>latitude() = float()</pre>



###<a name="type-longitude">longitude()</a>##



<pre>longitude() = float()</pre>



###<a name="type-position">position()</a>##



<pre>position() = {<a href="#type-latitude">latitude()</a>, <a href="#type-longitude">longitude()</a>, <a href="#type-timestamp">timestamp()</a>}</pre>



###<a name="type-protocol">protocol()</a>##



<pre>protocol() = binary()</pre>



###<a name="type-server_key">server_key()</a>##



<pre>server_key() = <a href="#type-key">key()</a></pre>



###<a name="type-timestamp">timestamp()</a>##



<pre>timestamp() = integer()</pre>



###<a name="type-user">user()</a>##



<pre>user() = binary()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_device_session-2">add_device_session/2</a></td><td>Register an active device session with a given protocol.</td></tr><tr><td valign="top"><a href="#add_http_session-0">add_http_session/0</a></td><td>Activate a Yaws server instance for the current application.</td></tr><tr><td valign="top"><a href="#check_queue-2">check_queue/2</a></td><td></td></tr><tr><td valign="top"><a href="#device_exists-1">device_exists/1</a></td><td>Check if the given device exists.</td></tr><tr><td valign="top"><a href="#get_account-0">get_account/0</a></td><td>Retrieves the current authorized account of the current process.</td></tr><tr><td valign="top"><a href="#get_cached_config-3">get_cached_config/3</a></td><td>Retrieves a cached config data set.</td></tr><tr><td valign="top"><a href="#login-2">login/2</a></td><td>Equivalent to <a href="#login-3"><tt>login(Account, User, true)</tt></a>.</td></tr><tr><td valign="top"><a href="#login-3">login/3</a></td><td>Authorize the current process as a given account and user.</td></tr><tr><td valign="top"><a href="#logout-0">logout/0</a></td><td>Equivalent to <a href="#logout-1"><tt>logout(true)</tt></a>.</td></tr><tr><td valign="top"><a href="#logout-1">logout/1</a></td><td>Log out the current process.</td></tr><tr><td valign="top"><a href="#lookup_device_keys-1">lookup_device_keys/1</a></td><td>Lookup the key pair associated with device.</td></tr><tr><td valign="top"><a href="#lookup_device_position-1">lookup_device_position/1</a></td><td>Lookup the last known position of the device.</td></tr><tr><td valign="top"><a href="#notification-4">notification/4</a></td><td></td></tr><tr><td valign="top"><a href="#queue_notification-4">queue_notification/4</a></td><td></td></tr><tr><td valign="top"><a href="#queue_reverse_request-4">queue_reverse_request/4</a></td><td></td></tr><tr><td valign="top"><a href="#remove_device_session-2">remove_device_session/2</a></td><td>Remove an active device session.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="add_device_session-2"></a>

###add_device_session/2##


<pre>add_device_session(Protocol::<a href="#type-protocol">protocol()</a>, DeviceID::<a href="#type-device_id">device_id()</a>) -> true</pre>
<br></br>




Register an active device session with a given protocol.

Device sessions indicate that the device is on-line and ready to send
and receive requests and notifications. Specifically, the current
process registers a [gproc property](https://github.com/uwiger/gproc/blob/master/doc/gproc.md#reg-1),
`{p, l, {exodm_rpc, active_device, ExtID, Protocol}}`, which
may be good to know while debugging. `ExtID` is an external representation
of the account name and device ID.<a name="add_http_session-0"></a>

###add_http_session/0##


<pre>add_http_session() -&gt; ok</pre>
<br></br>




Activate a Yaws server instance for the current application.

When a plugin application is reloaded/activated, this function can be called
to load (or reload) a HTTP server instance, as specified in the application
environment `{yaws_sconf, SConf}`, where `SConf` is a server configuration
list as described in [`http://yaws.hyber.org/embed.yaws`](http://yaws.hyber.org/embed.yaws).
The setting is read and expanded using
[setup:get_env/2](https://github.com/uwiger/setup/blob/master/doc/setup.md#get_env-2), which means that e.g.
the "variables" `$PRIV_DIR`, `$LIB_DIR`, `$HOME`, etc. can be used.<a name="check_queue-2"></a>

###check_queue/2##


`check_queue(Direction, DeviceID0) -> any()`

<a name="device_exists-1"></a>

###device_exists/1##


<pre>device_exists(DID::<a href="#type-device_id">device_id()</a>) -> boolean()</pre>
<br></br>




Check if the given device exists.

Returns `true` if the device exists, `false` otherwise.<a name="get_account-0"></a>

###get_account/0##


<pre>get_account() -> <a href="#type-account">account()</a></pre>
<br></br>




Retrieves the current authorized account of the current process.

This function will raise an exception if the current process is not
authorized (see [`login/2`](#login-2)).<a name="get_cached_config-3"></a>

###get_cached_config/3##


<pre>get_cached_config(ConfigSet::<a href="#type-config_set">config_set()</a>, Ref::integer(), DeviceID::<a href="#type-device_id">device_id()</a>) -> {ok, <a href="/Users/uwiger/FL/git/kvdb/doc/kvdb_conf.md#type-conf_tree">kvdb_conf:conf_tree()</a>} | {error, any()}</pre>
<br></br>




Retrieves a cached config data set.

This function is used in response to a `push-config-set` RPC.
When config set data is pushed, it is first stored in a cache, with a
reference for every affected device. Afterwards, a `push-config-set` RPC
is sent to each device. The data is in the form of a `kvdb_conf` config
config tree (see [kvdb_conf](/Users/uwiger/FL/git/kvdb/doc/kvdb_conf.md)).<a name="login-2"></a>

###login/2##


<pre>login(Account::<a href="#type-account">account()</a>, User::<a href="#type-user">user()</a>) -> boolean()</pre>
<br></br>


Equivalent to [`login(Account, User, true)`](#login-3).<a name="login-3"></a>

###login/3##


<pre>login(Account::<a href="#type-account">account()</a>, User::<a href="#type-user">user()</a>, Subscribe::boolean()) -> boolean()</pre>
<br></br>




Authorize the current process as a given account and user.



As a rule, it is good to log in as the user `<Account>-admin`, as it is
created automatically when the account is, and thus is guaranteed to exist.

If `Subscribe` is `true`, subscription on account deletion events is
initiated. If an account is deleted, the current process will receive a
message `{exodm_db_account, delete, AnyAccount}` (note: not just for the
current account). This allows the process to stop performing operations
that rely on the presence of `Account`.<a name="logout-0"></a>

###logout/0##


<pre>logout() -&gt; ok</pre>
<br></br>


Equivalent to [`logout(true)`](#logout-1).<a name="logout-1"></a>

###logout/1##


<pre>logout(Resubscribe::boolean()) -&gt; ok</pre>
<br></br>




Log out the current process.

If `Resubscribe` is true, this call will activate a subscription on account
add events. I.e. if an account is added/deleted, the current process will
receive messages of the form `{exodm_db_account, add, AcctName}`
(note: not just for the wanted account). This allows processes to be
started before the actual account has been created, and then automatically
pick up the account creation event and log in.<a name="lookup_device_keys-1"></a>

###lookup_device_keys/1##


<pre>lookup_device_keys(DeviceID::<a href="#type-device_id">device_id()</a>) -> <a href="#type-key_pair">key_pair()</a></pre>
<br></br>




Lookup the key pair associated with device.

If no keypair exists, `{<<0,0,0,0,0,0,0,0>>, <<0,0,0,0,0,0,0,0>>}`
is returned.<a name="lookup_device_position-1"></a>

###lookup_device_position/1##


<pre>lookup_device_position(DeviceID::<a href="#type-device_id">device_id()</a>) -> <a href="#type-position">position()</a></pre>
<br></br>




Lookup the last known position of the device.

If no position is stored, `{0.0, 0.0, 0}` is returned.<a name="notification-4"></a>

###notification/4##


`notification(Method, Elems, Env, DeviceID) -> any()`

<a name="queue_notification-4"></a>

###queue_notification/4##


`queue_notification(Module, Method, Elems, Env) -> any()`

<a name="queue_reverse_request-4"></a>

###queue_reverse_request/4##


`queue_reverse_request(Module, Method, Elems, Env) -> any()`

<a name="remove_device_session-2"></a>

###remove_device_session/2##


<pre>remove_device_session(DeviceID::<a href="#type-device_id">device_id()</a>, Protocol::<a href="#type-protocol">protocol()</a>) -> true</pre>
<br></br>




Remove an active device session.

This function removes a device session registered via
[`add_device_session/2`](#add_device_session-2). It will always succeed, even if there is no
such session.