-module(bench_map_to_record).

-compile([{parse_transform, rconv}]).

-export([
    from_map_rconv/1,
    bench_from_map_rconv/2,
    maps_merge/1,
    bench_maps_merge/2
]).

-record(test_record, {
    a = 42,
    b = "string" :: string(),
    c = false,
    d
}).

%% Benchmarks

from_map_rconv({input, _}) ->
    #{a => 666}.

bench_from_map_rconv(Map, _) ->
    rconv:from_map(Map, test_record).

maps_merge({input, _}) ->
    #{a => 666}.

bench_maps_merge(Map, _) ->
    Defaults = #{
        a => 42,
        b => "string",
        c => false,
        d => undefined
    },
    M = maps:merge(Defaults, Map),
    #test_record{
        a = map_get(a, M),
        b = map_get(b, M),
        c = map_get(c, M),
        d = map_get(d, M)
    }.
