-module(rconv).

-export([
    to_map/2,
    to_clean_map/3,
    from_map/2
]).

-export([
    parse_transform/2,
    format_error/1
]).

%% API stubs

%% @doc Constructs a map from a provided record.
-spec to_map(tuple(), atom()) -> map().
to_map(_Record, _RecordName) ->
    erlang:nif_error(<<"Add `-compile([{parse_transform, rconv}]).` to your module">>).

%% @doc Produces a filtered map. Skips values for which provided function returns false.
%% Note! Significantly slower than to_map/2 (but still faster than other methods).
%% Usage:
%% ```
%%   Filter = fun(V) -> V /= undefined andalso V /= null end,
%%   rconv:to_clean_map(Record, your_record_name, Filter).
%% '''
-spec to_clean_map(tuple(), atom(), fun((term()) -> boolean())) -> map().
to_clean_map(_Record, _RecordName, _FilterFun) ->
    erlang:nif_error(<<"Add `-compile([{parse_transform, rconv}]).` to your module">>).

%% @doc Constructs a record from a provided map. Absent fields will be filled with default values.
-spec from_map(map(), atom()) -> tuple().
from_map(_Map, _RecordName) ->
    erlang:nif_error(<<"Add `-compile([{parse_transform, rconv}]).` to your module">>).

%%     _____                    __
%%    |_   _|                  / _|
%%      | |_ __ __ _ _ __  ___| |_ ___  _ __ _ __ ___
%%      | | '__/ _` | '_ \/ __|  _/ _ \| '__| '_ ` _ \
%%      | | | | (_| | | | \__ \ || (_) | |  | | | | | |
%%      \_/_|  \__,_|_| |_|___/_| \___/|_|  |_| |_| |_|
%%

-type record_name() :: atom().
-type field_name() :: atom().
% type of a default value may differ from declared field type
% we only need to know the type of a default value to properly construct a record
-type default_value_type() :: atom().
-type default_value() :: term().

-record(state, {
    records = #{} :: #{record_name() => [{field_name(), default_value_type(), default_value()}]},
    errors = [] :: [{error, erl_parse:error_info()}]
}).

% -define(log(F, A), io:format(standard_error, "~n=======~n" F "~n=======~n", A)).
-define(log(F, A), ok).

parse_transform(Ast, _Opts) ->
    ?log("Ast: ~n~p", [Ast]),
    {NewAst, #state{errors = Errors}} = traverse(fun search_and_replace/2, #state{}, Ast),
    Errors ++ NewAst.

format_error({record_not_found, RecName}) ->
    io_lib:format("record '~p' not found", [RecName]);
format_error(bad_record_name) ->
    "record name argument should be an atom".

%% Internals

traverse(Fun, State, List) when is_list(List) ->
    lists:mapfoldl(
        fun(Node, St) ->
            traverse(Fun, St, Node)
        end,
        State,
        List
    );
traverse(Fun, State, Node) when is_tuple(Node) ->
    {Node2, State2} = Fun(Node, State),
    List = tuple_to_list(Node2),
    {Node3, State3} = traverse(Fun, State2, List),
    {list_to_tuple(Node3), State3};
traverse(_Fun, State, Node) ->
    {Node, State}.

search_and_replace(
    {attribute, _Line, record, {Name, AfFieldDecls}} = Node,
    #state{records = Records} = St
) ->
    ?log("record: ~p", [Node]),
    Fields = [record_field(F) || F <- AfFieldDecls],
    NewRecords = Records#{Name => Fields},
    {Node, St#state{records = NewRecords}};
search_and_replace( % map construction
    {call, Loc,
        {remote, _, {atom, _, ?MODULE}, {atom, _, to_map}},
        [RecVal, RecNameArg]} = AsIs,
    State
) ->
    case check_record(Loc, RecNameArg, State) of
        {ok, RecName, Fields} ->
            MapFields = [
                {map_field_assoc,
                    Loc,
                    {atom, Loc, F},
                    {record_field,
                        Loc,
                        RecVal,
                        RecName,
                        {atom, Loc, F}}}
                || {F, _Type, _DefValue} <- Fields
            ],
            MapExpr = {map, Loc, MapFields},
            {MapExpr, State};
        {error, NewState} ->
            {AsIs, NewState}
    end;
search_and_replace( % clean (filtered) map
    {call, Loc,
        {remote, _, {atom, _, ?MODULE}, {atom, _, to_clean_map}},
        [RecVal, RecNameArg, FilterFun]} = AsIs,
    State
) ->
    case check_record(Loc, RecNameArg, State) of
        {ok, RecName, Fields} ->
            MapFromList =
                {call, Loc,
                    {remote, Loc, {atom, Loc, maps}, {atom, Loc, from_list}},
                    [plus(Fields, RecVal, RecName, Loc, FilterFun)]},
            {MapFromList, State};
        {error, NewState} ->
            {AsIs, NewState}
    end;
search_and_replace( % record construction
    {call, Loc,
        {remote, _, {atom, _, ?MODULE}, {atom, _, from_map}},
        [MapVal, RecNameArg]} = AsIs,
    State
) ->
    case check_record(Loc, RecNameArg, State) of
        {ok, RecName, Fields} ->
            RecordFields = [
                {record_field,
                    Loc,
                    {atom, Loc, F},
                    {call, Loc,
                        % bench shows maps:get/3 ~2.5x faster than maps:merge/2 + map_get/2
                        % for small (4 fields) record
                        {remote, Loc, {atom, Loc, maps}, {atom, Loc, get}},
                        [{atom, Loc, F}, MapVal, {Type, Loc, DefValue}]}}
                || {F, Type, DefValue} <- Fields
            ],
            RecordExpr = {record, Loc, RecName, RecordFields},
            {RecordExpr, State};
        {error, NewState} ->
            {AsIs, NewState}
    end;
search_and_replace(Node, St) ->
    {Node, St}.

-spec record_field(erl_parse:af_field_decl()) ->
    {field_name(), default_value_type(), default_value()}.
record_field({record_field, _, {atom, _, F}}) when is_atom(F) ->
    {F, atom, undefined};
record_field({record_field, _, {atom, _, F}, {Type, _Pos, DefValue}}) when is_atom(F), is_atom(Type) ->
    {F, Type, DefValue};
record_field({typed_record_field, RecordField, _Type}) ->
    record_field(RecordField).

plus([F], RecVal, RecName, Loc, FilterFun) ->
    lc(F, RecVal, RecName, Loc, FilterFun);
plus([F | RestFields], RecVal, RecName, Loc, FilterFun) ->
    {op, Loc, '++', lc(F, RecVal, RecName, Loc, FilterFun), plus(RestFields, RecVal, RecName, Loc, FilterFun)};
plus([], _RecVal, _RecName, Loc, _FilterFun) ->
    {nil, Loc}. % empty record

lc({FieldName, _Type, _DefValue}, RecVal, RecName, Loc, FilterFun) ->
    {lc, Loc,
        {tuple, Loc, [
            {atom, Loc, FieldName},
            {record_field, Loc, RecVal, RecName, {atom, Loc, FieldName}}
        ]},
        [{call, Loc, FilterFun, [
            {record_field, Loc, RecVal, RecName, {atom, Loc, FieldName}}
        ]}]
    }.

check_record(_Loc, {atom, RecNameLoc, RecName}, #state{records = Records} = St) ->
    case maps:get(RecName, Records, undefined) of
        undefined ->
            {error, not_found(RecName, RecNameLoc, St)};
        Fields ->
            {ok, RecName, Fields}
    end;
check_record(Loc, _, St) ->
    {error, bad_record_name(Loc, St)}.

not_found(RecName, Loc, #state{errors = Errors} = St) ->
    Error = {error, {Loc, ?MODULE, {record_not_found, RecName}}},
    St#state{errors = [Error | Errors]}.

bad_record_name(Loc, #state{errors = Errors} = St) ->
    Error = {error, {Loc, ?MODULE, bad_record_name}},
    St#state{errors = [Error | Errors]}.
