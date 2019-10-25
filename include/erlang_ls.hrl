%%==============================================================================
%% Base Protocol
%%==============================================================================
%% := indicates a mandatory key
%% => indicates an optional key
%%==============================================================================

%%------------------------------------------------------------------------------
%% JSON-RPC Version
%%------------------------------------------------------------------------------
-define(JSONRPC_VSN, <<"2.0">>).
-type jsonrpc_vsn() :: binary().

%%------------------------------------------------------------------------------
%% Abstract Message
%%------------------------------------------------------------------------------
-type message() :: #{ jsonrpc := jsonrpc_vsn()
                    }.

%%------------------------------------------------------------------------------
%% Request Message
%%------------------------------------------------------------------------------
-type request() :: #{ jsonrpc := jsonrpc_vsn()
                    , id      := number() | binary()
                    , method  := binary()
                    , params  => [any()] | map()
                    }.
%%------------------------------------------------------------------------------
%% Response Message
%%------------------------------------------------------------------------------
-type response() :: #{ jsonrpc := jsonrpc_vsn()
                     , id      := number() | binary() | null
                     , result  => any()
                     , error   => error(any())
                     }.

-type error(Type) :: #{ code    := number()
                      , message := binary()
                      , data    => Type
                      }.

%% Defined by JSON RPC
-define(ERR_PARSE_ERROR            , -32700).
-define(ERR_INVALID_REQUEST        , -32600).
-define(ERR_METHOD_NOT_FOUND       , -32601).
-define(ERR_INVALID_PARAMS         , -32602).
-define(ERR_INTERNAL_ERROR         , -32603).
-define(ERR_SERVER_ERROR_START     , -32099).
-define(ERR_SERVER_ERROR_END       , -32000).
-define(ERR_SERVER_NOT_INITIALIZED , -32002).
-define(ERR_UNKNOWN_ERROR_CODE     , -32001).

%% Defined by the protocol
-define(ERR_REQUEST_CANCELLED      , -32800).

%%------------------------------------------------------------------------------
%% Notification Message
%%------------------------------------------------------------------------------
-type notification(Method, Params) :: #{ jsonrpc := jsonrpc_vsn()
                                       , method  := Method
                                       , params  => Params
                                       }.

%%------------------------------------------------------------------------------
%% Cancellation Support
%%------------------------------------------------------------------------------
-type cancel_params() :: #{ id := number() | binary()
                          }.

%%==============================================================================
%% Language Server Protocol
%%==============================================================================

%%------------------------------------------------------------------------------
%% Position
%%------------------------------------------------------------------------------
-type position() :: #{ line      := number()
                     , character := number()
                     }.

%%------------------------------------------------------------------------------
%% Range
%%------------------------------------------------------------------------------
-type range() :: #{ start := position()
                  , 'end' := position()
                  }.

%%------------------------------------------------------------------------------
%% Location
%%------------------------------------------------------------------------------
-type location() :: #{ uri   := uri()
                     , range := range()
                     }.

%%------------------------------------------------------------------------------
%% Diagnostic
%%------------------------------------------------------------------------------
-type diagnostic() :: #{ range              := range()
                       , severity           => severity()
                       , code               => number() | binary()
                       , source             => binary()
                       , message            := binary()
                       , relatedInformation => [related_info()]
                       }.

-define(DIAGNOSTIC_ERROR   , 1).
-define(DIAGNOSTIC_WARNING , 2).
-define(DIAGNOSTIC_INFO    , 3).
-define(DIAGNOSTIC_HINT    , 4).

-type severity() :: ?DIAGNOSTIC_ERROR
                  | ?DIAGNOSTIC_WARNING
                  | ?DIAGNOSTIC_INFO
                  | ?DIAGNOSTIC_HINT.

-type related_info() :: #{ location := location()
                         , message  := binary()
                         }.

%%------------------------------------------------------------------------------
%% Command
%%------------------------------------------------------------------------------
-type command() :: #{ title     := binary()
                    , command   := binary()
                    , arguments => [any()]
                    }.

%%------------------------------------------------------------------------------
%% Insert Text Format
%%------------------------------------------------------------------------------

-define(INSERT_TEXT_FORMAT_PLAIN_TEXT, 1).
-define(INSERT_TEXT_FORMAT_SNIPPET,    2).

%%------------------------------------------------------------------------------
%% Text Edit
%%------------------------------------------------------------------------------
-type text_edit() :: #{ range   := range()
                      , newText := binary()
                      }.

%%------------------------------------------------------------------------------
%% Text Document Edit
%%------------------------------------------------------------------------------
-type text_document_edit() :: #{ textDocument := versioned_text_document_id()
                               , edits        := [text_edit()]
                               }.

%%------------------------------------------------------------------------------
%% Workspace Edit
%%------------------------------------------------------------------------------
-type workspace_edit() :: #{ changes         => #{ binary() := [text_edit()]
                                                 }
                           , documentChanges => [text_document_edit()]
                           }.

%%------------------------------------------------------------------------------
%% Text Document Identifier
%%------------------------------------------------------------------------------
-type text_document_id() :: #{ uri := uri() }.

%%------------------------------------------------------------------------------
%% Text Document Item
%%------------------------------------------------------------------------------
-type text_document_item() :: #{ uri        := uri()
                               , languageId := binary()
                               , version    := number()
                               , text       := binary()
                               }.

%%------------------------------------------------------------------------------
%% Text Document Sync Kind
%%------------------------------------------------------------------------------

-define(TEXT_DOCUMENT_SYNC_KIND_NONE, 0).
-define(TEXT_DOCUMENT_SYNC_KIND_FULL, 1).
-define(TEXT_DOCUMENT_SYNC_KIND_INCREMENTAL, 2).

%%------------------------------------------------------------------------------
%% Text Document Sync Kind
%%------------------------------------------------------------------------------

-define(COMPLETION_TRIGGER_KIND_INVOKED,                    1).
-define(COMPLETION_TRIGGER_KIND_CHARACTER,                  2).
-define(COMPLETION_TRIGGER_KIND_FOR_INCOMPLETE_COMPLETIONS, 3).

%%------------------------------------------------------------------------------
%% Versioned Text Document Identifier
%%------------------------------------------------------------------------------
-type versioned_text_document_id() :: #{ version := number() | null
                                       }.

%%------------------------------------------------------------------------------
%% Text Document Position Params
%%------------------------------------------------------------------------------
-type text_document_position_params() :: #{ textDocument := text_document_id()
                                          , position     := position()
                                          }.

%%------------------------------------------------------------------------------
%% Document Fiter
%%------------------------------------------------------------------------------
-type document_filter() :: #{ language => binary()
                            , scheme   => binary()
                            , pattern  => binary()
                            }.

-type document_selector() :: [document_filter()].

%%------------------------------------------------------------------------------
%% Markup Content
%%------------------------------------------------------------------------------
-define(PLAINTEXT , plaintext).
-define(MARKDOWN  , markdown).

-type markup_kind() :: ?PLAINTEXT
                     | ?MARKDOWN.

-type markup_content() :: #{ kind  := markup_kind()
                           , value := binary()
                           }.

%%==============================================================================
%% Actual Protocol
%%==============================================================================

%%------------------------------------------------------------------------------
%% Initialize Request
%%------------------------------------------------------------------------------
-type workspace_folder() :: #{ uri  => uri()
                             , name => binary()
                             }.

-define(COMPLETION_ITEM_KIND_TEXT, 1).
-type completion_item_kind() :: ?COMPLETION_ITEM_KIND_TEXT.

-define(CODE_ACTION_KIND_QUICKFIX, 1).
-type code_action_kind() :: ?CODE_ACTION_KIND_QUICKFIX.

-type initialize_params() :: #{ processId             := number() | null
                              , rootPath              => binary() | null
                              , rootUri               := uri() | null
                              , initializationOptions => any()
                              , capabilities          := client_capabilities()
                              , trace                 => off
                                                       | messages
                                                       | verbose
                              , workspaceFolders      => [workspace_folder()]
                                                       | null
                              }.

-type client_capabilities() ::
        #{ workspace    => workspace_client_capabilities()
         , textDocument => text_document_client_capabilities()
         , experimental => any()
         }.

-type workspace_client_capabilities() ::
        #{ applyEdit => boolean()
         , workspaceEdit =>
             #{ documentChanges => boolean()
              }
         , didChangeConfiguration =>
             #{ dynamicRegistration => boolean()
              }
         , didChangeWatchedFiles =>
             #{ dynamicRegistration => boolean()
              }
         , symbol =>
             #{ dynamicRegistration => boolean()
              , symbolKind =>
                  #{ valueSet => [symbol_kind()]
                   }
              }
         , executeCommand =>
             #{ dynamicRegistration => boolean()
              }
         , workspaceFolders => boolean()
         , configuration => boolean()
         }.

-type text_document_client_capabilities() ::
        #{ synchronization =>
             #{ dynamicRegistration => boolean()
              , willSave => boolean()
              , willSaveWaitUntil => boolean()
              , didSave => boolean()
              }
         , completion =>
             #{ dynamicRegistration => boolean()
              , completionItem =>
                  #{ snippetSupport => boolean()
                   , commitCharactersSupport => boolean()
                   , documentationFormat => markup_kind()
                   , deprecatedSupport => boolean()
                   }
              , completionItemKind =>
                  #{ valueSet => [completion_item_kind()]
                   }
              , contextSupport => boolean()
              }
         , hover =>
             #{ dynamicRegistration => boolean()
              , contentFormat => [markup_kind()]
              }
         , signatureHelp =>
             #{ dynamicRegistration => boolean()
              , signatureInformation =>
                  #{ documentationFormat => [markup_kind()]
                   }
              }
         , references =>
             #{ dynamicRegistration => boolean()
              }
         , documentHighlight =>
             #{ dynamicRegistration => boolean()
              }
         , documentSymbol =>
             #{ dynamicRegistration => boolean()
              , symbolKind =>
                  #{ valueSet => [symbol_kind()]
                   }
              }
         , formatting =>
             #{ dynamicRegistration => boolean()
              }
         , rangeFormatting =>
             #{ dynamicRegistration => boolean()
              }
         , onTypeFormatting =>
             #{ dynamicRegistration => boolean()
              }
         , definition =>
             #{ dynamicRegistration => boolean()
              }
         , typeDefinition =>
             #{ dynamicRegistration => boolean()
              }
         , implementation =>
             #{ dynamicRegistration => boolean()
              }
         , codeAction =>
             #{ dynamicRegistration => boolean()
              , codeActionLiteralSupport =>
                  #{ codeActionKind :=
                       #{ valueSet := [code_action_kind()]
                        }
                   }
              }
         , codeLens =>
             #{ dynamicRegistration => boolean()
              }
         , documentLink =>
             #{ dynamicRegistration => boolean()
              }
         , colorProvider =>
             #{ dynamicRegistration => boolean()
              }
         , rename =>
             #{ dynamicRegistration => boolean()
              }
         , publishDiagnostics =>
             #{ relatedInformation => boolean()
              }
         }.

-type server_capabilities() :: #{
                          }.

-type initialize_result() :: #{ capabilities => server_capabilities()
                              }.

%%------------------------------------------------------------------------------
%% Initialize Notification
%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Shutdown Request
%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% Exit Notification
%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% ShowMessage Notification
%%-----------------------------------------------------------------------------
-type show_message_notification() :: notification( show_message_method()
                                                 , show_message_params()
                                                 ).

-type show_message_method() :: 'window/showMessage'.
-type show_message_params() :: #{ type    := show_message_type()
                                , message := binary()
                                }.

-define(MESSAGE_TYPE_ERROR   , 1).
-define(MESSAGE_TYPE_WARNING , 2).
-define(MESSAGE_TYPE_INFO    , 3).
-define(MESSAGE_TYPE_LOG     , 4).

-type show_message_type() :: ?MESSAGE_TYPE_ERROR
                           | ?MESSAGE_TYPE_WARNING
                           | ?MESSAGE_TYPE_INFO
                           | ?MESSAGE_TYPE_LOG.

%%------------------------------------------------------------------------------
%% Symbol Kinds
%%------------------------------------------------------------------------------

-define(SYMBOLKIND_FILE           , 1).
-define(SYMBOLKIND_MODULE         , 2).
-define(SYMBOLKIND_NAMESPACE      , 3).
-define(SYMBOLKIND_PACKAGE        , 4).
-define(SYMBOLKIND_CLASS          , 5).
-define(SYMBOLKIND_METHOD         , 6).
-define(SYMBOLKIND_PROPERTY       , 7).
-define(SYMBOLKIND_FIELD          , 8).
-define(SYMBOLKIND_CONSTRUCTOR    , 9).
-define(SYMBOLKIND_ENUM           , 10).
-define(SYMBOLKIND_INTERFACE      , 11).
-define(SYMBOLKIND_FUNCTION       , 12).
-define(SYMBOLKIND_VARIABLE       , 13).
-define(SYMBOLKIND_CONSTANT       , 14).
-define(SYMBOLKIND_STRING         , 15).
-define(SYMBOLKIND_NUMBER         , 16).
-define(SYMBOLKIND_BOOLEAN        , 17).
-define(SYMBOLKIND_ARRAY          , 18).
-define(SYMBOLKIND_OBJECT         , 19).
-define(SYMBOLKIND_KEY            , 20).
-define(SYMBOLKIND_NULL           , 21).
-define(SYMBOLKIND_ENUM_MEMBER    , 22).
-define(SYMBOLKIND_STRUCT         , 23).
-define(SYMBOLKIND_EVENT          , 24).
-define(SYMBOLKIND_OPERATOR       , 25).
-define(SYMBOLKIND_TYPE_PARAMETER , 26).

-type symbol_kind() :: ?SYMBOLKIND_FILE
                     | ?SYMBOLKIND_MODULE
                     | ?SYMBOLKIND_NAMESPACE
                     | ?SYMBOLKIND_PACKAGE
                     | ?SYMBOLKIND_CLASS
                     | ?SYMBOLKIND_METHOD
                     | ?SYMBOLKIND_PROPERTY
                     | ?SYMBOLKIND_FIELD
                     | ?SYMBOLKIND_CONSTRUCTOR
                     | ?SYMBOLKIND_ENUM
                     | ?SYMBOLKIND_INTERFACE
                     | ?SYMBOLKIND_FUNCTION
                     | ?SYMBOLKIND_VARIABLE
                     | ?SYMBOLKIND_CONSTANT
                     | ?SYMBOLKIND_STRING
                     | ?SYMBOLKIND_NUMBER
                     | ?SYMBOLKIND_BOOLEAN
                     | ?SYMBOLKIND_ARRAY
                     | ?SYMBOLKIND_OBJECT
                     | ?SYMBOLKIND_KEY
                     | ?SYMBOLKIND_NULL
                     | ?SYMBOLKIND_ENUM_MEMBER
                     | ?SYMBOLKIND_STRUCT
                     | ?SYMBOLKIND_EVENT
                     | ?SYMBOLKIND_OPERATOR
                     | ?SYMBOLKIND_TYPE_PARAMETER.

-type symbol_information() :: #{ name          := binary()
                               , kind          := symbol_kind()
                               , deprecated    => boolean()
                               , location      := location()
                               , containerName => binary()
                               }.

%%------------------------------------------------------------------------------
%% Signatures
%%------------------------------------------------------------------------------
-type parameter_information() :: #{ label         := binary()
                                  , documentation => binary()
                                  }.
-type signature_information() :: #{ label         := binary()
                                  , documentation => binary()
                                  , parameters    => [parameter_information()]
                                  }.
-type signature_help()        :: #{ signatures := [signature_information()]
                                  , active_signature => number()
                                  , active_parameters => number()
                                  }.

%%------------------------------------------------------------------------------
%% Internals
%%------------------------------------------------------------------------------
-type pos()       :: {integer(), integer()}.
-type uri()       :: binary().
-type poi_kind()  :: application
                   | behaviour
                   | define
                   | exports_entry
                   | function
                   | implicit_fun
                   | import_entry
                   | include
                   | include_lib
                   | macro
                   | module
                   | record
                   | record_access
                   | record_expr
                   | spec
                   | type_application
                   | type_definition.
-type poi_range() :: #{ from := pos(), to := pos() }.
-type poi()       :: #{ kind  => poi_kind()
                      , data  => any()
                      , range := poi_range()
                      }.
