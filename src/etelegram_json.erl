-module(etelegram_json).

-export([encode/1, decode/1]).

encode(Term) ->
    jsx:encode(Term).

decode(JSON) ->
    jsx:decode(JSON, [{labels, atom}]).
