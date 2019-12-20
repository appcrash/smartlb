# What is smart-lb

It is a simple tcp proxy that proxying tcp stream by incoming connection trait.

The trait can be anything from information from socket, packet, like:
  - source ip
  - initial packet content by regex matching
  - ......

Once trait resolved, proxy the connection to backend servers.


# Config File

lb.conf is the main config file. it should be in the same directory of lb working dir.

For simplicity, it just contains the erlang terms that will be evaluated at runtime.

> {matcher,Match_Pattern,Keyword,Backend_List}.
>
> {matcher,...}.
>
> ...


Example:

```erlang
{matcher,"(?i)host:\\s*(\\w+)\\s*\r\n","www.google.com",[{"127.0.0.1",8080}]}.
```

It will search "Host" header in the first 512 bytes of every incoming connection, and if header value is "www.google.com" proxies connection to 127.0.0.1:8080. In fact, the incoming connection is not necessarily http procotol, it can be any binary procotol. You can put lots of matcher here, and first matched one wins just like "filter" in some http framework.
