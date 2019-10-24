%%==============================================================================
%% Code Navigation
%%==============================================================================
-module(erlang_ls_code_navigation).

%%==============================================================================
%% Exports
%%==============================================================================

%% API
-export([ goto_definition/2 ]).

%%==============================================================================
%% Includes
%%==============================================================================
-include("erlang_ls.hrl").

%%==============================================================================
%% API
%%==============================================================================

-spec goto_definition(uri(), erlang_ls_poi:poi()) ->
   {ok, uri(), erlang_ls_poi:poi()} | {error, any()}.
goto_definition( _Uri
               , #{ kind := Kind, data := {M, F, A} }
               ) when Kind =:= application;
                      Kind =:= implicit_fun ->
  %% TODO: Abstract name from completion_index
  {ok, Uri} = erlang_ls_db:find(completion_index, M),
  find(Uri, function, {F, A});
goto_definition( Uri
               , #{ kind := Kind, data := {F, A}}
               ) when Kind =:= application;
                      Kind =:= implicit_fun;
                      Kind =:= exports_entry ->
  find(Uri, function, {F, A});
goto_definition(_Uri, #{ kind := behaviour, data := Behaviour }) ->
  {ok, Uri} = erlang_ls_db:find(completion_index, Behaviour),
  find(Uri, module, Behaviour);
goto_definition(_Uri, #{ kind := import_entry, data := {M, F, A}}) ->
  %% TODO: Abstract name from completion_index
  {ok, Uri} = erlang_ls_db:find(completion_index, M),
  find(Uri, function, {F, A});
goto_definition(Uri, #{ kind := macro, data := Define }) ->
  find(Uri, define, Define);
goto_definition(Uri, #{ kind := record_access
                      , data := {Record, _}}) ->
  find(Uri, record, Record);
goto_definition(Uri, #{ kind := record_expr, data := Record }) ->
  find(Uri, record, Record);
goto_definition(_Uri, #{ kind := Kind, data := Include }
               ) when Kind =:= include;
                      Kind =:= include_lib ->
  M = include_string_to_atom(Kind, Include),
  case erlang_ls_db:find(completion_index, M) of
    %% TODO: Index the header itself
    {ok, Uri} ->
      {ok, Uri, #{range => #{from => {1, 1}, to => {1, 1}}}};
    not_found ->
      {error, not_found}
  end;
goto_definition(Uri, #{ kind := type_application, data := {Type, _} }) ->
  find(Uri, type_definition, Type);
goto_definition(_Filename, _) ->
  {error, not_found}.

%% TODO: Move poi/kind to hrl
-spec find([uri()], erlang_ls_poi:kind(), any()) ->
   {ok, uri(), erlang_ls_poi:poi()} | {error, not_found}.
find([], _Kind, _Data) ->
  {error, not_found};
find([Uri|Uris0], Kind, Data) ->
  case erlang_ls_db:find(documents, Uri) of
    {ok, Document} ->
      POIs = erlang_ls_document:points_of_interest(Document, [Kind], Data),
      case POIs of
        [] ->
          Uris = include_uris(Document) ++ Uris0,
          find(lists:usort(Uris), Kind, Data);
        Definitions ->
          {ok, Uri, lists:last(Definitions)}
      end;
    not_found ->
      find(Uris0, Kind, Data)
  end;
find(Uri, Kind, Data) ->
  find([Uri], Kind, Data).

-spec include_uris(erlang_ls_document:document()) -> [uri()].
include_uris(Document) ->
  POIs = erlang_ls_document:points_of_interest(Document, [ include
                                                         , include_lib
                                                         ]),
  F = fun(#{ kind := Kind, data := String }, Acc) ->
          Module = include_string_to_atom(Kind, String),
          case erlang_ls_db:find(completion_index, Module) of
            {ok, Uri} -> [Uri|Acc];
            not_found -> Acc
          end
      end,
  lists:foldl(F, [], POIs).

-spec include_string_to_atom('include' | 'include_lib', string()) -> atom().
include_string_to_atom(include, String) ->
  list_to_atom(string:trim(String, both, [$"]));
include_string_to_atom(include_lib, String) ->
  list_to_atom(lists:last(filename:split(string:trim(String, both, [$"])))).
