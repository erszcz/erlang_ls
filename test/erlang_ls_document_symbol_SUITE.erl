-module(erlang_ls_document_symbol_SUITE).

%% CT Callbacks
-export([ suite/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        , all/0
        ]).

%% Test cases
-export([ symbols/1
        ]).


-include("erlang_ls.hrl").

%%==============================================================================
%% Includes
%%==============================================================================
-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

%%==============================================================================
%% Types
%%==============================================================================
-type config() :: [{atom(), any()}].

%%==============================================================================
%% CT Callbacks
%%==============================================================================
-spec suite() -> [tuple()].
suite() ->
  [{timetrap, {seconds, 30}}].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  {ok, Started} = application:ensure_all_started(erlang_ls),
  [{started, Started}|Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(Config) ->
  [application:stop(App) || App <- ?config(started, Config)],
  ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(TestCase, Config) ->
  erlang_ls_test_utils:init_per_testcase(TestCase, Config).

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_TestCase, _Config) ->
  ok.

-spec all() -> [atom()].
all() ->
  erlang_ls_test_utils:all(?MODULE).

%%==============================================================================
%% Testcases
%%==============================================================================
-spec symbols(config()) -> ok.
symbols(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  #{result := Symbols} = erlang_ls_client:document_symbol(Uri),
  Expected = [ #{ kind => Kind,
                  location =>
                    #{ range =>
                         #{ 'end' => #{character => ToC, line => ToL},
                            start => #{character => FromC, line => FromL}
                          },
                       uri => NewUri
                     },
                  name => Name
                } || {Kind, NewUri, Name, {FromL, FromC}, {ToL, ToC}}
                       <- lists:append([modules(Config), functions(Config)])],
  ?assertEqual(length(Expected), length(Symbols)),
  Pairs = lists:zip(lists:sort(Expected), lists:sort(Symbols)),
  [?assertEqual(E, S) || {E, S} <- Pairs],
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
functions(Config) ->
  Uri = ?config(code_navigation_uri, Config),
  [ {?SYMBOLKIND_FUNCTION, Uri, <<"callback_a/0">>, {27, 0}, {27, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_a/0">>, {20, 0}, {20, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_b/0">>, {24, 0}, {24, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_c/0">>, {30, 0}, {30, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_d/0">>, {38, 0}, {38, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_e/0">>, {41, 0}, {41, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_f/0">>, {46, 0}, {46, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_g/1">>, {49, 0}, {49, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_h/0">>, {55, 0}, {55, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_i/0">>, {59, 0}, {59, 10}}
  , {?SYMBOLKIND_FUNCTION, Uri, <<"function_i/0">>, {61, 0}, {61, 10}}
  ].

modules(Config) ->
  [ {?SYMBOLKIND_MODULE, ?config(behaviour_uri, Config),             <<"behaviour_a">>,           {0, 0}, {0, 0}}
  , {?SYMBOLKIND_MODULE, ?config(code_navigation_extra_uri, Config), <<"code_navigation_extra">>, {0, 0}, {0, 0}}
  , {?SYMBOLKIND_MODULE, ?config(code_navigation_uri, Config),       <<"code_navigation">>,       {0, 0}, {0, 0}}
  ].
