-module(els_config_indexing).

-include("els_core.hrl").

-export([ default_config/0 ]).

%% Getters
-export([ get_skip_generated_files/0
        , get_generated_files_tag/0
        , get_incremental/0
        ]).

-type config() :: #{ string() => string() }.

-spec default_config() -> config().
default_config() ->
  #{ "skip_generated_files" => default_skip_generated_files()
   , "generated_files_tag" => default_generated_files_tag()
   , "incremental" => default_incremental()
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

-spec get_incremental() -> boolean().
get_incremental() ->
  Value = maps:get("incremental",
                   els_config:get(indexing),
                   default_incremental()),
  normalize_boolean(Value).

-spec default_skip_generated_files() -> string().
default_skip_generated_files() ->
  "false".

-spec default_generated_files_tag() -> string().
default_generated_files_tag() ->
  "@generated".

-spec default_incremental() -> string().
default_incremental() ->
  "false".

-spec normalize_boolean(boolean() | string()) -> boolean().
normalize_boolean("true") -> true;
normalize_boolean("false") -> false;
normalize_boolean(Bool) when is_boolean(Bool) -> Bool.
