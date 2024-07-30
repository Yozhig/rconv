-module(compile_time_errors_SUITE).

-include_lib("stdlib/include/assert.hrl").

-behaviour(ct_suite).
-export([
    all/0
]).

%% Tests
-export([
    norecord/1,
    format_error/1,
    success/1
]).

%% ct_suite callbacks

all() ->
    [
        norecord,
        format_error,
        success
    ].

%% Tests

norecord(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, "norecord"),
    ?assertEqual(
        {
            error,
            [
                {File ++ ".erl", [
                    {{15, 23}, rconv, {record_not_found, norecord}},
                    {{18, 5}, rconv, bad_record_name},
                    {{22, 29}, rconv, {record_not_found, another_absent_record}},
                    {{25, 25}, rconv, {record_not_found, norecord}}
                ]}
            ],
            []
        },
        compile:file(File, [return])
    ).

format_error(_Config) ->
    ?assertEqual(
        "record 'norecord' not found",
        lists:flatten(rconv:format_error({record_not_found, norecord}))
    ),
    ?assertEqual(
        "record name argument should be an atom",
        lists:flatten(rconv:format_error(bad_record_name))
    ).

success(Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    File = filename:join(DataDir, "success"),
    ?assertEqual(
        {ok, success, []},
        compile:file(File, [return])
    ).
