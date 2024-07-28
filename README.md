rconv
=====

Parse transform for fast record to map and map to record conversions without boilerplate for Erlang.

Usage
-----

Add dependency to your rebar.config
```erlang
    {deps, [
        {rconv, {git, "git@github.com:Yozhig/rconv.git", {tag, "0.1.0"}}},
    ]}
```
and compile option for parse transform at the top of the source file
```erlang
    -module(your_module_name).

    -compile([{parse_transform, rconv}]).
```
or to an `erl_opts`.

Now you can use rconv:to_map/2 and rconv:from_map/2 which will be transformed at the compile time
```erlang
    Map = rconv:to_map(Record, your_record_name),
    Record = rconv:from_map(Map, your_record_name)
```
See [unit tests](test/rconv_test.erl) for sample usage.
