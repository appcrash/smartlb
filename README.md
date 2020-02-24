# What is smart-lb

It is a simple tcp proxy that proxying tcp stream by incoming connection trait.

The trait can be anything from properties of socket/packet, like:
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
> {default_matcher,Timeout,Backend_List}
>
> ...


Example:

```erlang
{matcher,"(?i)host:\\s*(\\w+)\\s*\r\n","www.google.com",[{"127.0.0.1",8080}]}.
{default_matcher,100,[{"127.0.0.1",9090}]}.
```

It will search "Host" header in the first 512 bytes of every incoming connection, and if header value is "www.google.com" proxies connection to 127.0.0.1:8080, otherwise the default 127.0.0.1:9090 is selected once packet size reaches threhold or 100ms times out. The timeout setting for default matcher is necessary as some protocols would not send enough info in its initial packet(handshaking). Timeout prevents the proxy from holding the buffered data too long to connect a remote host. In fact, the incoming connection is not necessarily http procotol, it can be any binary procotol. You can put lots of matcher here, and first matched one wins just like "filter" in some http framework.

# Environment Variables

Following variables can be overridden by sys.config file for application **lb**.
  
  - port(integer): the proxy listening port, default is 10080
  
