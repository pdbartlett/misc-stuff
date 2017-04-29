-module(proctest).
-export([test/0, do_something/0]).

test() ->
  spawn(proctest, do_something, []).

do_something() ->
  io:format("Hello world!~n").