%%% This module must not compile and parse transform should emit errors at compile time

-module(norecord).

-compile([{parse_transform, rconv}]).

-export([
    to_map/1,
    to_map_bad_arg/1,
    to_clean_map/1,
    from_map/1
]).

to_map(Rec) ->
    rconv:to_map(Rec, norecord).

to_map_bad_arg(Rec) ->
    rconv:to_map(Rec, []).

to_clean_map(Rec) ->
    Filter = fun(V) -> V /= undefined end,
    rconv:to_clean_map(Rec, another_absent_record, Filter).

from_map(Map) ->
    rconv:from_map(Map, norecord).
