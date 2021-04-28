![build status][1]

[1]: https://www.travis-ci.org/appcrash/smartlb.svg?branch=master

# What is smartlb

It is a smiple tcp/udp proxy/forwarder based on a set of rules.

The rules works by inspect the initial contents of packet and determine the destination of the incoming connection being proxied to.


# Flow config File

Flow file is the main config file. it should be configed in the *sys.config*, default is "lb.conf".

For simplicity, it just contains the erlang terms that will be evaluated at runtime. The whole config is an array of tuples whose keys are of **matcher**,**rule**,**backend**,**flow**:

> tuples() :: [{key(),value()}]
>
> key() :: matcher | rule | backend | flow
>
> value() :: map()

Config file is in form of:

> [
>
> {matcher, [ matchers ... ]},
>
> {backend, [ backends ... ]},
>
> {rule, [ rules ... ]},
>
> {flow, [ flows ... ]}
>
> ].

## matcher

Each Mathcer has an id which can be referenced by rules, and has a type of **regex** or **function**. The goal of matcher is extracting values from packet binary, like specific http header value, protocol vesion etc. With regex type stdlib *re* of erlang is used for matching with **pattern** and **capture** key as argument. The reuslt of matcher is a list of *captured* group in the regex pattern from the packet binary. You can also define a custom function with packet binary as sole argument and return a list as result.

## backend

Each backend is a list of host to be proxied to. Backend id is referenced by the target of a **flow**. Once a backend is determined by a flow, the packet would be proxied/forwarded to that backend. Backend can be of type *round-robin* or *weighted* among the host list.

## rule

Each rule has an id as well as a referenced matcher id. The rule use the result of the matcher id as the input to the **condition** function. The condition function has two arguments: original packet binary and the matcher result(a list of captured group). We separate matcher and rule because the matcher result can be cached and used by many rules in a single matching request. The condition function must return:

- true

The rule matched, final result of a flow.

- need_more

This means not matched by this flow and requires more data to match in next time, final result of a flow. It is used in a stream protocol like TCP, when data can be accumulated enough until successfully matched in a limited initial size. The datagram protocol like UDP has no such trait so never use *need_more* rule for udp forwarder.

- any thing other than above

This means not matched by this rule, the flow can continue to next one, not a final result.

## flow

The main configration that define the behaviour of the proxy. It is just a organization of rules and targets. The packet binary walk through from first flow item to the end. Each flow item use a rule to find the target of this packet. It works in a way of:

- rule matched(true is returned from rule function), go to the target(select the backend).

- rule nomatch but need_more is returned, matching is ended and the client gets the result instructing wait for more incoming data and try again.

- rule not match, go to next flow in flow list and repeat above actions. If this is already the final flow, return nomatch to client.


# How it works

The whole configuration file contains a list of tuples. Once this list is evaluated by erlang, smartlb use matcher_builder to start a new process trying to compile it. Each matcher is converted to

> fun(binary()) -> list().

Each rule is converted to

rule_func = #rule_item.rule_func (user provided)
fun(Data,[Group1,Group2,...]) -> true | need_more | other().

Each backend is convert to selection function:

> fun(FlowState::map()) -> {SelectedHost,FlowState}.

Each flow is convert to:

> fun(Data::binary(),MatcherCache::map(),FlowState::map()) ->
>
> FlowResult:: {match,Host,FlowState} | {need_more,FlowState} | {nomatch,MatcherCache,FlowState}.

Then the matcher worker execute every matching request by each flow item and send the result to requesting process by {match_result,Result}.

# Application Configuration
- udp_enable(boolean): udp proxy enabled or not
- udp_mtu(integer): the maximum size of forwared udp payload
- udp_ip(string): the udp server listening on ip
- upd_port(integer): the udp server listening on port

- tcp_enable(boolean): tcp proxy enabled or not
- tcp_port(integer): the tcp proxy listening port
- metric_http_port(integer): the http port to receive metric data

- flow_file(string): the main flow configuration file
- matcher_worker(integer): the number of process exectuing matching request
