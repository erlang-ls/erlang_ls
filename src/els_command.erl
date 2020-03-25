%%==============================================================================
%% Command
%%==============================================================================
-module(els_command).

%%==============================================================================
%% API
%%==============================================================================
-export([ with_prefix/1
        , without_prefix/1
        ]).

%%==============================================================================
%% Constructors
%%==============================================================================

-export([ make_command/3 ]).

%%==============================================================================
%% Type Definitions
%%==============================================================================

-type command() :: #{ title     := binary()
                    , command   := command_id()
                    , arguments => [any()]
                    }.
-type command_id() :: binary().
-export_type([ command/0
             , command_id/0
             ]).

%%==============================================================================
%% API
%%==============================================================================

%% @doc Add a server-unique prefix to a command.
-spec with_prefix(command_id()) -> command_id().
with_prefix(Id) ->
  Prefix = server_prefix(),
  <<Prefix/binary, ":", Id/binary>>.

%% @doc Strip a server-unique prefix from a command.
-spec without_prefix(command_id()) -> command_id().
without_prefix(Id0) ->
  case binary:split(Id0, <<":">>) of
    [_, Id] -> Id;
    [Id] -> Id
  end.

%%==============================================================================
%% Constructors
%%==============================================================================

-spec make_command(binary(), command_id(), [map()]) -> command().
make_command(Title, CommandId, Args) ->
  #{ title     => Title
   , command   => with_prefix(CommandId)
   , arguments => Args
   }.

%%==============================================================================
%% Internal Functions
%%==============================================================================

%% @doc Generate a prefix unique to this running erlang_ls server.
%%
%% This is needed because some clients have a global namespace for all
%% registered commands, and we need to be able to run multiple
%% erlang_ls instances at the same time against a single client.
-spec server_prefix() -> binary().
server_prefix() ->
   els_utils:to_binary(os:getpid()).
