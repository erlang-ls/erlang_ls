-module(els_config_indexing).

-include("els_core.hrl").

-export([ default_config/0 ]).

%% Getters
-export([ get_skip_generated_files/0
        , get_generated_files_tag/0
        ]).

-type config() :: #{ string() => string() }.

-spec default_config() -> config().
default_config() ->
  #{ "skip_generated_files" => default_skip_generated_files()
   , "generated_files_tag" => default_generated_files_tag()
   }.

-spec get_skip_generated_files() -> boolean().
get_skip_generated_files() ->
  Value = maps:get("skip_generated_files",
                   els_config:get(indexing),
                   default_skip_generated_files()),
  normalize_boolean(Value).

-spec get_generated_files_tag() -> string().
get_generated_files_tag() ->
  maps:get("generated_files_tag",
           els_config:get(indexing),
           default_generated_files_tag()).

-spec default_skip_generated_files() -> string().
default_skip_generated_files() ->
  "false".

-spec default_generated_files_tag() -> string().
default_generated_files_tag() ->
  "@generated".

-spec normalize_boolean(boolean() | string()) -> boolean().
normalize_boolean(Value) when is_list(Value) ->
  normalize_boolean(list_to_atom(Value));
normalize_boolean(Value) when is_atom(Value) ->
  Value.
