-module(erlang_ls_document_symbol_provider).

-behaviour(erlang_ls_provider).

-export([ handle_request/2
        , is_enabled/0
        , setup/1
        , teardown/0
        ]).

-include("erlang_ls.hrl").

%%==============================================================================
%% erlang_ls_provider functions
%%==============================================================================

-spec is_enabled() -> boolean().
is_enabled() ->
  true.

-spec setup(map()) -> erlang_ls_provider:state().
setup(_Config) ->
  #{}.

-spec handle_request(any(), erlang_ls_provider:state()) ->
  {any(), erlang_ls_provider:state()}.
handle_request({document_symbol, Params}, State) ->
  #{ <<"textDocument">> := #{ <<"uri">> := Uri}} = Params,
  Modules   = modules(),
  Functions = functions(Uri),
  Symbols = lists:append(Modules, Functions),
  case Symbols of
    [] -> {null, State};
    _  -> {Symbols, State}
  end.

-spec teardown() -> ok.
teardown() ->
  ok.

%%==============================================================================
%% Internal Functions
%%==============================================================================
-spec functions(uri()) -> [map()].
functions(Uri) ->
  {ok, Document} = erlang_ls_db:find(documents, Uri),
  POIs = erlang_ls_document:points_of_interest(Document, [function]),
  [ #{ name => function_name(F, A)
     , kind => ?SYMBOLKIND_FUNCTION
     , location => #{ uri   => Uri
                    , range => erlang_ls_protocol:range(Range)
                    }
     } || #{data := {F, A}, range := Range} <- POIs ].

-spec modules() -> [map()].
modules() ->
  Entries = erlang_ls_db:list(completion_index),
  Range = #{from => {1, 1}, to => {1, 1}},
  [ #{ name => atom_to_binary(Module, utf8)
     , kind => ?SYMBOLKIND_MODULE
     , location => #{ uri => Uri
                    , range => erlang_ls_protocol:range(Range)
                    }
       %% TODO: Do not index header files together with modules
     } || {Module, Uri} <- Entries, filename:extension(Uri) =:= <<".erl">>].

-spec function_name(atom(), non_neg_integer()) -> binary().
function_name(F, A) ->
  list_to_binary(io_lib:format("~p/~p", [F, A])).
