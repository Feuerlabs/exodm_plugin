

# Module exodm_plugin #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


API functions for Exosense server plugin development.

<a name="description"></a>

## Description ##


This module contains support functions for Exosense server (exodm) plugins.

<a name="types"></a>

## Data Types ##




### <a name="type-account">account()</a> ###



<pre><code>
account() = binary()
</code></pre>





### <a name="type-client_key">client_key()</a> ###



<pre><code>
client_key() = <a href="#type-key">key()</a>
</code></pre>





### <a name="type-config_set">config_set()</a> ###



<pre><code>
config_set() = binary()
</code></pre>





### <a name="type-device_id">device_id()</a> ###



<pre><code>
device_id() = binary()
</code></pre>





### <a name="type-json">json()</a> ###



<pre><code>
json() = <a href="#type-json_struct">json_struct()</a> | <a href="#type-json_array">json_array()</a> | <a href="#type-json_string">json_string()</a> | <a href="#type-json_int">json_int()</a>
</code></pre>





### <a name="type-json_array">json_array()</a> ###



<pre><code>
json_array() = {array, [<a href="#type-json">json()</a>]}
</code></pre>





### <a name="type-json_int">json_int()</a> ###



<pre><code>
json_int() = integer() | <a href="#type-json_string">json_string()</a>
</code></pre>





### <a name="type-json_string">json_string()</a> ###



<pre><code>
json_string() = string() | binary()
</code></pre>





### <a name="type-json_struct">json_struct()</a> ###



<pre><code>
json_struct() = {struct, [<a href="#type-json">json()</a>]}
</code></pre>





### <a name="type-key">key()</a> ###



<pre><code>
key() = binary()
</code></pre>





### <a name="type-key_pair">key_pair()</a> ###



<pre><code>
key_pair() = {<a href="#type-client_key">client_key()</a>, <a href="#type-server_key">server_key()</a>}
</code></pre>





### <a name="type-latitude">latitude()</a> ###



<pre><code>
latitude() = float()
</code></pre>





### <a name="type-longitude">longitude()</a> ###



<pre><code>
longitude() = float()
</code></pre>





### <a name="type-position">position()</a> ###



<pre><code>
position() = {<a href="#type-latitude">latitude()</a>, <a href="#type-longitude">longitude()</a>, <a href="#type-timestamp">timestamp()</a>}
</code></pre>





### <a name="type-protocol">protocol()</a> ###



<pre><code>
protocol() = binary()
</code></pre>





### <a name="type-rpc_reply">rpc_reply()</a> ###



<pre><code>
rpc_reply() = [{<a href="#type-yang_id">yang_id()</a>, <a href="#type-yang_value">yang_value()</a>}]
</code></pre>





### <a name="type-server_key">server_key()</a> ###



<pre><code>
server_key() = <a href="#type-key">key()</a>
</code></pre>





### <a name="type-timestamp">timestamp()</a> ###



<pre><code>
timestamp() = integer()
</code></pre>





### <a name="type-user">user()</a> ###



<pre><code>
user() = binary()
</code></pre>





### <a name="type-yang_id">yang_id()</a> ###



<pre><code>
yang_id() = atom()
</code></pre>





### <a name="type-yang_value">yang_value()</a> ###



<pre><code>
yang_value() = any()
</code></pre>



 erlang representation of a yang-specified value
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_device_session-2">add_device_session/2</a></td><td>Register an active device session with a given protocol.</td></tr><tr><td valign="top"><a href="#add_http_session-0">add_http_session/0</a></td><td>Activate a Yaws server instance for the current application.</td></tr><tr><td valign="top"><a href="#check_queue-2">check_queue/2</a></td><td>Check the notification queue belonging to device.</td></tr><tr><td valign="top"><a href="#device_exists-1">device_exists/1</a></td><td>Check if the given device exists.</td></tr><tr><td valign="top"><a href="#get_account-0">get_account/0</a></td><td>Retrieves the current authorized account of the current process.</td></tr><tr><td valign="top"><a href="#get_cached_config-3">get_cached_config/3</a></td><td>Retrieves a cached config data set.</td></tr><tr><td valign="top"><a href="#json_rpc-2">json_rpc/2</a></td><td>Process an RPC as if it had come from the web.</td></tr><tr><td valign="top"><a href="#load_yang_module-3">load_yang_module/3</a></td><td></td></tr><tr><td valign="top"><a href="#login-2">login/2</a></td><td>Equivalent to <a href="#login-3"><tt>login(Account, User, true)</tt></a>.</td></tr><tr><td valign="top"><a href="#login-3">login/3</a></td><td>Authorize the current process as a given account and user.</td></tr><tr><td valign="top"><a href="#logout-0">logout/0</a></td><td>Equivalent to <a href="#logout-1"><tt>logout(true)</tt></a>.</td></tr><tr><td valign="top"><a href="#logout-1">logout/1</a></td><td>Log out the current process.</td></tr><tr><td valign="top"><a href="#lookup_device_attr-2">lookup_device_attr/2</a></td><td>Lookup an attribute value in a device object.</td></tr><tr><td valign="top"><a href="#lookup_device_keys-1">lookup_device_keys/1</a></td><td>Lookup the key pair associated with device.</td></tr><tr><td valign="top"><a href="#lookup_device_position-1">lookup_device_position/1</a></td><td>Lookup the last known position of the device.</td></tr><tr><td valign="top"><a href="#notification-4">notification/4</a></td><td>Send a notification or RPC associated with <code>DeviceID</code>.</td></tr><tr><td valign="top"><a href="#push_config_result-4">push_config_result/4</a></td><td></td></tr><tr><td valign="top"><a href="#queue_notification-4">queue_notification/4</a></td><td>Equivalent to <a href="#queue_notification-4"><tt>queue_notification(Module, Method, Elems,
get_device_id(Env))</tt></a>.</td></tr><tr><td valign="top"><a href="#queue_notification-5">queue_notification/5</a></td><td>Queue a message for notification "upstream" for DeviceID.</td></tr><tr><td valign="top"><a href="#queue_reverse_request-4">queue_reverse_request/4</a></td><td></td></tr><tr><td valign="top"><a href="#queue_reverse_request-5">queue_reverse_request/5</a></td><td>Queue an RPC for delivery "upstream" for DeviceID.</td></tr><tr><td valign="top"><a href="#remove_device_session-2">remove_device_session/2</a></td><td>Remove an active device session.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_device_session-2"></a>

### add_device_session/2 ###


<pre><code>
add_device_session(Protocol::<a href="#type-protocol">protocol()</a>, DeviceID::<a href="#type-device_id">device_id()</a>) -&gt; true
</code></pre>

<br></br>



Register an active device session with a given protocol.


Device sessions indicate that the device is on-line and ready to send
and receive requests and notifications. Specifically, the current
process registers a [gproc property](https://github.com/uwiger/gproc/blob/master/doc/gproc.md#reg-1),
`{p, l, {exodm_rpc, active_device, ExtID, Protocol}}`, which
may be good to know while debugging. `ExtID` is an external representation
of the account name and device ID.
<a name="add_http_session-0"></a>

### add_http_session/0 ###


<pre><code>
add_http_session() -&gt; ok
</code></pre>

<br></br>



Activate a Yaws server instance for the current application.


When a plugin application is reloaded/activated, this function can be called
to load (or reload) a HTTP server instance, as specified in the application
environment `{yaws_sconf, SConf}`, where `SConf` is a server configuration
list as described in [`http://yaws.hyber.org/embed.yaws`](http://yaws.hyber.org/embed.yaws).
The setting is read and expanded using
[setup:get_env/2](https://github.com/uwiger/setup/blob/master/doc/setup.md#get_env-2), which means that e.g.
the "variables" `$PRIV_DIR`, `$LIB_DIR`, `$HOME`, etc. can be used.
<a name="check_queue-2"></a>

### check_queue/2 ###


<pre><code>
check_queue(Direction::to_device | from_device, DeviceID0::<a href="#type-device_id">device_id()</a>) -&gt; ok
</code></pre>

<br></br>



Check the notification queue belonging to device.



All notifications and RPCs to/from devices are pushed device-specific queues,
one for each device and direction (to or from device). Whenever a queue goes
from empty to nonempty, a dispatch process is spawned to try to deliver the
message. In the case of the `from_device` queue, delivery should normally
succeed, so that queue dispatch normally doesn't need to be triggered.


However, it is expected that a custom protocol manager triggers the
`to_device` whenever a device comes online.
<a name="device_exists-1"></a>

### device_exists/1 ###


<pre><code>
device_exists(DID::<a href="#type-device_id">device_id()</a>) -&gt; boolean()
</code></pre>

<br></br>



Check if the given device exists.


Returns `true` if the device exists, `false` otherwise.
<a name="get_account-0"></a>

### get_account/0 ###


<pre><code>
get_account() -&gt; <a href="#type-account">account()</a>
</code></pre>

<br></br>



Retrieves the current authorized account of the current process.


This function will raise an exception if the current process is not
authorized (see [`login/2`](#login-2)).
<a name="get_cached_config-3"></a>

### get_cached_config/3 ###


<pre><code>
get_cached_config(ConfigSet::<a href="#type-config_set">config_set()</a>, Ref::integer(), DeviceID::<a href="#type-device_id">device_id()</a>) -&gt; {ok, <a href="/Users/uwiger/FL/git/kvdb/doc/kvdb_conf.md#type-conf_tree">kvdb_conf:conf_tree()</a>} | {error, any()}
</code></pre>

<br></br>



Retrieves a cached config data set.


This function is used in response to a `push-config-set` RPC.
When config set data is pushed, it is first stored in a cache, with a
reference for every affected device. Afterwards, a `push-config-set` RPC
is sent to each device. The data is in the form of a `kvdb_conf` config
config tree (see [kvdb_conf](/Users/uwiger/FL/git/kvdb/doc/kvdb_conf.md)).
<a name="json_rpc-2"></a>

### json_rpc/2 ###


<pre><code>
json_rpc(_Method::binary(), Elems::<a href="#type-json_struct">json_struct()</a>) -&gt; {true, <a href="#type-json">json()</a>} | {false, any()}
</code></pre>

<br></br>



Process an RPC as if it had come from the web.


This function simulates a JSON-RPC call, e.g. `{call, Method, Elems}`, as if
it came from the web. `Elems` and the reply are on the same form as is
returned by `json2:decode_string/1`. The request goes through the same
validation and queueing as normal JSON-RPC requests. The difference is that
the HTTP transport part is eliminated.
<a name="load_yang_module-3"></a>

### load_yang_module/3 ###

`load_yang_module(Area, ModuleName, Bin) -> any()`


<a name="login-2"></a>

### login/2 ###


<pre><code>
login(Account::<a href="#type-account">account()</a>, User::<a href="#type-user">user()</a>) -&gt; boolean()
</code></pre>

<br></br>


Equivalent to [`login(Account, User, true)`](#login-3).
<a name="login-3"></a>

### login/3 ###


<pre><code>
login(Account::<a href="#type-account">account()</a>, User::<a href="#type-user">user()</a>, Subscribe::boolean()) -&gt; boolean()
</code></pre>

<br></br>



Authorize the current process as a given account and user.



As a rule, it is good to log in as the user `<Account>-admin`, as it is
created automatically when the account is, and thus is guaranteed to exist.


If `Subscribe` is `true`, subscription on account deletion events is
initiated. If an account is deleted, the current process will receive a
message `{exodm_db_account, delete, AnyAccount}` (note: not just for the
current account). This allows the process to stop performing operations
that rely on the presence of `Account`.
<a name="logout-0"></a>

### logout/0 ###


<pre><code>
logout() -&gt; ok
</code></pre>

<br></br>


Equivalent to [`logout(true)`](#logout-1).
<a name="logout-1"></a>

### logout/1 ###


<pre><code>
logout(Resubscribe::boolean()) -&gt; ok
</code></pre>

<br></br>



Log out the current process.


If `Resubscribe` is true, this call will activate a subscription on account
add events. I.e. if an account is added/deleted, the current process will
receive messages of the form `{exodm_db_account, add, AcctName}`
(note: not just for the wanted account). This allows processes to be
started before the actual account has been created, and then automatically
pick up the account creation event and log in.
<a name="lookup_device_attr-2"></a>

### lookup_device_attr/2 ###


<pre><code>
lookup_device_attr(_Attr::binary(), DeviceID::<a href="#type-device_id">device_id()</a>) -&gt; [{_Attr, any()}] | []
</code></pre>

<br></br>



Lookup an attribute value in a device object.


If the device doesn't exist, or the requested attribute is not stored,
the empty list (`[]`) is returned.
<a name="lookup_device_keys-1"></a>

### lookup_device_keys/1 ###


<pre><code>
lookup_device_keys(DeviceID::<a href="#type-device_id">device_id()</a>) -&gt; <a href="#type-key_pair">key_pair()</a>
</code></pre>

<br></br>



Lookup the key pair associated with device.


If no keypair exists, `{<<0,0,0,0,0,0,0,0>>, <<0,0,0,0,0,0,0,0>>}`
is returned.
<a name="lookup_device_position-1"></a>

### lookup_device_position/1 ###


<pre><code>
lookup_device_position(DeviceID::<a href="#type-device_id">device_id()</a>) -&gt; <a href="#type-position">position()</a>
</code></pre>

<br></br>



Lookup the last known position of the device.


If no position is stored, `{0.0, 0.0, 0}` is returned.
<a name="notification-4"></a>

### notification/4 ###


<pre><code>
notification(_Method::binary(), _Elems::[{_Key::binary(), _Value::any()}], _Env::[{atom(), any()}], DeviceID::<a href="#type-device_id">device_id()</a>) -&gt; ok | <a href="#type-rpc_reply">rpc_reply()</a> | {error, any()}
</code></pre>

<br></br>



Send a notification or RPC associated with `DeviceID`.


This function maps `Method` to a Yang-specified Notification or RPC, using
the Yang modules associated with `DeviceID`. The request is processed as
if it had originated from the device. If the method is an RPC, the reply
is validated against the Yang spec and converted to internal form: a list
of `{Key, Value}` tuples corresponding to the 'output' elements in the spec.
<a name="push_config_result-4"></a>

### push_config_result/4 ###

`push_config_result(X1, Cfg, Ref, DeviceID) -> any()`


<a name="queue_notification-4"></a>

### queue_notification/4 ###

`queue_notification(Module, Method, Elems, Env) -> any()`

Equivalent to [`queue_notification(Module, Method, Elems,get_device_id(Env))`](#queue_notification-4).
<a name="queue_notification-5"></a>

### queue_notification/5 ###


<pre><code>
queue_notification(_Module::binary(), _Method::binary(), _Elems::[{binary() | atom(), any()}], _Env::[{atom(), any()}], DeviceID::<a href="#type-device_id">device_id()</a>) -&gt; {ok, _Queue::binary(), _AbsKey::any()} | {error, any()}
</code></pre>

<br></br>



Queue a message for notification "upstream" for DeviceID.


The message will be queued in the `from_device` queue, and dispatched
as soon as possible.
<a name="queue_reverse_request-4"></a>

### queue_reverse_request/4 ###

`queue_reverse_request(Module, Method, Elems, Env) -> any()`


<a name="queue_reverse_request-5"></a>

### queue_reverse_request/5 ###


<pre><code>
queue_reverse_request(_Module::binary(), _Method::binary(), _Elems::[{binary() | atom(), any()}], _Env::[{atom(), any()}], DeviceID::<a href="#type-device_id">device_id()</a>) -&gt; {ok, _Queue::binary(), _AbsKey::any()} | {error, any()}
</code></pre>

<br></br>



Queue an RPC for delivery "upstream" for DeviceID.


The message will be queued in the `from_device` queue, and dispatched
as soon as possible.
<a name="remove_device_session-2"></a>

### remove_device_session/2 ###


<pre><code>
remove_device_session(DeviceID::<a href="#type-device_id">device_id()</a>, Protocol::<a href="#type-protocol">protocol()</a>) -&gt; true
</code></pre>

<br></br>



Remove an active device session.


This function removes a device session registered via
[`add_device_session/2`](#add_device_session-2). It will always succeed, even if there is no
such session.
