-module(rconv_test).

-include_lib("eunit/include/eunit.hrl").

-compile([{parse_transform, rconv}]).

-record(test_record, {
    a = 1 :: integer(),
    b :: string() | binary() | undefined,
    c = true,
    d
}).

-record(empty, {}).

to_map_test() ->
    Rec = #test_record{b = "some_string"},
    Map = rconv:to_map(Rec, test_record),
    ?assertEqual(
        #{
            a => 1,
            b => "some_string",
            c => true,
            d => undefined
        },
        Map
    ).

empty_to_map_test() ->
    _Rec = #empty{},
    Map = rconv:to_map(_Rec, empty),
    ?assertEqual(#{}, Map).

from_map_test() ->
    Map = #{
        a => 42,
        b => <<"bla-bla">>
    },
    Rec = rconv:from_map(Map, test_record),
    ?assertEqual(
        #test_record{
            a = 42,
            b = <<"bla-bla">>,
            c = true,
            d = undefined
        },
        Rec
    ).

from_empty_map_test() ->
    ?assertEqual(
        #test_record{
            a = 1,
            b = undefined,
            c = true,
            d = undefined
        },
        rconv:from_map(#{}, test_record)
    ).

to_clean_map_test() ->
    Rec = #test_record{b = "other string"},
    Filter = fun(V) -> V /= undefined end,
    Map = rconv:to_clean_map(Rec, test_record, Filter),
    ?assertEqual(
        #{
            a => 1,
            b => "other string",
            c => true
        },
        Map
    ).

empty_to_clean_map_test() ->
    _Rec = #empty{},
    _Filter = fun(V) -> V /= undefined end,
    Map = rconv:to_clean_map(Rec, empty, _Filter),
    ?assertEqual(#{}, Map).
