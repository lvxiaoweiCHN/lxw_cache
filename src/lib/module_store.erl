-module(module_store).

-export([init/0, insert/2, delete/1, lookup/1]).

init() ->
    ok.

lookup(Key) ->
    Mod = key_to_module(Key),
    try Mod:term() of
        {Value, Current_time} ->
            {ok, Value, Current_time}
    catch error:undef ->
        {error, not_found}
    end.

insert(Key, Value) ->
    Current_time = time_utils:get_current_seconds(),
    Mod = key_to_module(Key),
    Bin = compile(Mod, {Value, Current_time}),
    code:purge(Mod),
    code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
    ok.

delete(Key) ->
    Mod = key_to_module(Key),
    code:purge(Mod),
    code:delete(Mod).

key_to_module(Key) ->
    list_to_atom(lists:concat([?MODULE, "_", Key])).

-spec compile(atom(), any()) -> binary().
compile(Module, T) ->
    {ok, Module, Bin} = compile:forms(forms(Module, T), [verbose, report_errors]),
    Bin.

-spec forms(atom(), any()) -> [erl_syntax:syntaxTree()].
forms(Module, T) ->
    [erl_syntax:revert(X) || X <- term_to_abstract(Module, term, T)].

-spec term_to_abstract(atom(), atom(), any()) -> [erl_syntax:syntaxTree()].
term_to_abstract(Module, Getter, T) ->
    [%% -module(Module).
     erl_syntax:attribute(
       erl_syntax:atom(module),
       [erl_syntax:atom(Module)]),
     %% -export([Getter/0]).
     erl_syntax:attribute(
       erl_syntax:atom(export),
       [erl_syntax:list(
         [erl_syntax:arity_qualifier(
            erl_syntax:atom(Getter),
            erl_syntax:integer(0))])]),
     %% Getter() -> T.
     erl_syntax:function(
       erl_syntax:atom(Getter),
       [erl_syntax:clause([], none, [erl_syntax:abstract(T)])])].