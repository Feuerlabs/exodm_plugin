

# Module exodm_plugin_lib #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Helper functions for plugin development.
__Authors:__ Ulf Wiger ([`ulf@feuerlabs.com`](mailto:ulf@feuerlabs.com)).
<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#data_to_json-3">data_to_json/3</a></td><td>Exosense wrapper around <code>yang_json:data_to_json(Spec,Env,Elems)</code>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="data_to_json-3"></a>

### data_to_json/3 ###


<pre><code>
data_to_json(Spec::YangOutputSpec, Env, Data::MsgElems) -&gt; JSONParams
</code></pre>

<br></br>



Exosense wrapper around `yang_json:data_to_json(Spec,Env,Elems)`.



This function emulates the semantics added by Exosense Server on top of
`yang_json:data_to_json/3`, and is meant to be used in unit testing.


Specifically, it checks for the presence of the "rpc-status" and
"rpc-status-string" elements, and assigns a corresponding value to
"rpc-status-string", in line with the behavior described in the
`exosense.yang` specification.
