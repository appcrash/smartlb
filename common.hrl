-record(matcher,{
  regex_mp,
  keyword,
  addr
}).

-record(default_matcher,{
  timeout,
  addr
}).

-record(match_state,{
  matcher_list
}).