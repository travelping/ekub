-module(ekub_core_tools).

-export([
    resource/2,
    query/1
]).

resource(Path, Args) ->
    lists:flatten(io_lib:format(Path, Args)).

query(Options) ->
    lists:map(fun option/1, Options).

option({Name, Value}) ->
    {underscore_atom_to_camel_case_string(Name), to_string(Value)}.

underscore_atom_to_camel_case_string(Atom) ->
    case string:split(atom_to_list(Atom), "_", all) of
        [First] -> First;
        [First|Rest] -> lists:flatten([First|[[H-32|T] || [H|T] <- Rest]])
    end.

to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
to_string(Number) when is_number(Number) -> integer_to_list(Number);
to_string(String) when is_list(String) -> String.
