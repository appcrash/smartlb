-record(matcher,{
  regex_mp,
  keyword,
  addr
}).

-record(default_matcher,{
  timeout,
  addr
}).

-record(matcher_state,{
  index = 0,
  backend_number
}).

-record(trait_state,{
  matcher_list,
  matcher_state_table
}).


-define(UDP_SRC_IP,"127.0.0.1").
-define(UDP_SRC_PORT,10070).
-define(TCP_SRC_PORT,10080).
-define(TCP_INIT_TIMEOUT,5000).
-define(METRIC_HTTP_PORT,10090).
-define(PREFORK,1).
