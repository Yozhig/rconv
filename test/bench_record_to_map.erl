-module(bench_record_to_map).

-compile([{parse_transform, rconv}]).

-compile({inline, [
    rec_to_plist/1,
    rec_to_plist/3
]}).

-export([
    hands/1,
    bench_hands/2,
    zip/1,
    bench_zip/2,
    fold_tuple_map/1,
    bench_fold_tuple_map/2,
    fold_tuple_plist/1,
    bench_fold_tuple_plist/2,
    rconv/1,
    bench_rconv/2,
    to_clean_map_zip/1,
    bench_to_clean_map_zip/2,
    to_clean_map_foldl/1,
    bench_to_clean_map_foldl/2,
    to_clean_map_foldl_list/1,
    bench_to_clean_map_foldl_list/2,
    to_clean_map_compr/1,
    bench_to_clean_map_compr/2,
    to_clean_map_list_compr/1,
    bench_to_clean_map_list_compr/2,
    to_clean_map_plus/1,
    bench_to_clean_map_plus/2,
    to_clean_map_rconv/1,
    bench_to_clean_map_rconv/2
]).

-record(bench_rec, {a, b, c, d, e, f, g}).

-define(FIELDS, [
    {a, #bench_rec.a},
    {b, #bench_rec.b},
    {c, #bench_rec.c},
    {d, #bench_rec.d},
    {e, #bench_rec.e},
    {f, #bench_rec.f},
    {g, #bench_rec.g}
]).

%% Benchmarks

hands({input, _}) ->
    fish().

bench_hands(Rec, _) ->
    #{
        a => Rec#bench_rec.a,
        b => Rec#bench_rec.b,
        c => Rec#bench_rec.c,
        d => Rec#bench_rec.d,
        e => Rec#bench_rec.e,
        f => Rec#bench_rec.f,
        g => Rec#bench_rec.g
    }.

zip({input, _}) ->
    fish().

bench_zip(Rec, _) ->
    maps:from_list(lists:zip(record_info(fields, bench_rec), tl(tuple_to_list(Rec)))).

fold_tuple_map({input, _}) ->
    fish().

bench_fold_tuple_map(Rec, _) ->
    rec_to_map(Rec).

fold_tuple_plist({input, _}) ->
    fish().

bench_fold_tuple_plist(Rec, _) ->
    maps:from_list(rec_to_plist(Rec)).

rconv({input, _}) ->
    fish().

bench_rconv(Rec, _) ->
    rconv:to_map(Rec, bench_rec).

to_clean_map_zip({input, _}) ->
    fish().

bench_to_clean_map_zip(Rec, _) ->
    maps:from_list([
        Pair ||
        {_, V} = Pair <- lists:zip(record_info(fields, bench_rec), tl(tuple_to_list(Rec))),
        V /= undefined
    ]).

to_clean_map_foldl({input, _}) ->
    fish().

bench_to_clean_map_foldl(Rec, _) ->
    lists:foldl(
        fun({K, I}, Acc) ->
            case element(I, Rec) of
                undefined ->
                    Acc;
                V ->
                    Acc#{K => V}
            end
        end,
        #{},
        ?FIELDS
    ).

to_clean_map_foldl_list({input, _}) ->
    fish().

bench_to_clean_map_foldl_list(Rec, _) ->
    List = lists:foldl(
        fun({K, I}, Acc) ->
            case element(I, Rec) of
                undefined ->
                    Acc;
                V ->
                    [{K, V} | Acc]
            end
        end,
        [],
        ?FIELDS
    ),
    maps:from_list(List).

to_clean_map_compr({input, _}) ->
    fish().

bench_to_clean_map_compr(Rec, _) ->
    #{
        K => element(I, Rec) ||
        {K, I} <- ?FIELDS,
        element(I, Rec) /= undefined
    }.

to_clean_map_list_compr({input, _}) ->
    fish().

bench_to_clean_map_list_compr(Rec, _) ->
    maps:from_list([
        {K, element(I, Rec)} ||
        {K, I} <- ?FIELDS,
        element(I, Rec) /= undefined
    ]).

to_clean_map_plus({input, _}) ->
    fish().

bench_to_clean_map_plus(Rec, _) ->
    FF = fun(V) -> V /= undefined end,
    maps:from_list(
        [{a, Rec#bench_rec.a} || FF(Rec#bench_rec.a)] ++
        [{b, Rec#bench_rec.b} || FF(Rec#bench_rec.b)] ++
        [{c, Rec#bench_rec.c} || FF(Rec#bench_rec.c)] ++
        [{d, Rec#bench_rec.d} || FF(Rec#bench_rec.d)] ++
        [{e, Rec#bench_rec.e} || FF(Rec#bench_rec.e)] ++
        [{f, Rec#bench_rec.f} || FF(Rec#bench_rec.f)] ++
        [{g, Rec#bench_rec.g} || FF(Rec#bench_rec.g)]
    ).
    % TODO: why the code below is slower?
    % maps:from_list(
    %     [{a, Rec#bench_rec.a} || Rec#bench_rec.a /= undefined] ++
    %     [{b, Rec#bench_rec.b} || Rec#bench_rec.b /= undefined] ++
    %     [{c, Rec#bench_rec.c} || Rec#bench_rec.c /= undefined] ++
    %     [{d, Rec#bench_rec.d} || Rec#bench_rec.d /= undefined] ++
    %     [{e, Rec#bench_rec.e} || Rec#bench_rec.e /= undefined] ++
    %     [{f, Rec#bench_rec.f} || Rec#bench_rec.f /= undefined] ++
    %     [{g, Rec#bench_rec.g} || Rec#bench_rec.g /= undefined]
    % ).

to_clean_map_rconv({input, _}) ->
    fish().

bench_to_clean_map_rconv(Rec, _) ->
    Filter = fun(V) -> V /= undefined end,
    rconv:to_clean_map(Rec, bench_rec, Filter).

%% Internals

fish() ->
    #bench_rec{a = 300, b = true, c = <<"bytes">>, d = [{a, 1}, {b, 2}]}.

-define(REC_INFO(R), list_to_tuple([R | record_info(fields, R)])). % should be a literal

rec_to_plist(R) ->
    rec_to_plist(R, 2, ?REC_INFO(bench_rec)).

rec_to_plist(R, N, Info) when N =< tuple_size(R) ->
    [{element(N, Info), element(N, R)} | rec_to_plist(R, N + 1, Info)];
rec_to_plist(_, _, _) ->
    [].

rec_to_map(R) ->
    rec_to_map(R, 2, ?REC_INFO(bench_rec), #{}).

rec_to_map(R, N, Info, Acc) when N =< tuple_size(R) ->
    rec_to_map(R, N + 1, Info, Acc#{element(N, Info) => element(N, R)});
rec_to_map(_, _, _, Acc) ->
    Acc.
