-record(matcher,{
  regex_mp,
  keyword,
  addr
}).

-record(default_matcher,{
  addr
}).

-record(match_state,{
  matcher_list
}).