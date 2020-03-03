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


-define(SRC_PORT,10080).
-define(METRIC_HTTP_PORT,10090).
-define(PREFORK,1).
