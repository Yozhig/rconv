-module(rconv).

-export([
    to_map/2,
    to_clean_map/3,
    from_map/2
]).

-export([parse_transform/2]).

-type record_name() :: atom().
-type field_name() :: atom().
% type of a default value may differ from declared field type
% we only need to know the type of a default value to properly construct a record
-type default_value_type() :: atom().
-type default_value() :: term().

-record(state, {
    records = #{} :: #{record_name() => [{field_name(), default_value_type(), default_value()}]}
}).

% -define(log(F, A), io:format(standard_error, "~n=======~n" F "~n=======~n", A)).
-define(log(F, A), ok).

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

parse_transform(Ast, _Opts) ->
    % ?log("Ast: ~n~p", [Ast]),
    {NewAst, _} = traverse(fun search_and_replace/2, #state{}, Ast),
    NewAst.

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
        [RecVal, {atom, _, RecName}]} = Node,
    #state{records = Records} = St
) ->
    case maps:get(RecName, Records, undefined) of
        undefined -> % FIXME: emit error at compile time
            ?log("WARNING: ~p not found", [RecName]),
            {Node, St};
        Fields ->
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
            {MapExpr, St}
    end;
search_and_replace( % clean (filtered) map
    {call, Loc,
        {remote, _, {atom, _, ?MODULE}, {atom, _, to_clean_map}},
        [RecVal, RecNameAtom, FilterFun]} = Node,
    #state{records = Records} = St
) ->
    {atom, _, RecName} = RecNameAtom,
    case maps:get(RecName, Records, undefined) of
        undefined -> % FIXME: emit error at compile time
            ?log("WARNING: ~p not found", [RecName]),
            {Node, St};
        Fields ->
            MapFromList =
                {call, Loc,
                    {remote, Loc, {atom, Loc, maps}, {atom, Loc, from_list}},
                    [plus(Fields, RecVal, RecName, Loc, FilterFun)]},
            {MapFromList, St}
    end;
search_and_replace( % record construction
    {call, Loc,
        {remote, _, {atom, _, ?MODULE}, {atom, _, from_map}},
        [MapVal, {atom, _, RecName}]} = Node,
    #state{records = Records} = St
) ->
    case maps:get(RecName, Records, undefined) of
        undefined -> % FIXME: emit error at compile time
            ?log("WARNING: ~p not found", [RecName]),
            {Node, St};
        Fields ->
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
            {RecordExpr, St}
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
