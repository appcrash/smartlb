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

-record(metric_state,{
  incoming_conn_total_number = 0,
  incoming_conn_failed_number = 0
}).
