-module(success).

-compile([{parse_transform, rconv}]).

-export([
    to_map/1,
    to_clean_map/1,
    empty_to_clean_map/1,
    from_map/1
]).

-record(success, {
    a :: term(),
    b = false,
    c
}).

-record(empty, {}).

to_map(R) ->
    rconv:to_map(R, success).

to_clean_map(R) ->
    Filter = fun(V) -> V /= undefined end,
    rconv:to_clean_map(R, success, Filter).

empty_to_clean_map(#empty{} = _E) ->
    % just a strange corner case, not an example
    rconv:to_clean_map(_E, empty, fun(_) -> true end).

from_map(M) ->
    rconv:from_map(M, success).
