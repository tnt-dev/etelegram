-module(etelegram_utils).

-export([ensure_started/1, to_binary/1, is_file_exists/1]).

ensure_started(App) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok
    end.

to_binary(A) when is_atom(A) ->
    to_binary(atom_to_list(A));
to_binary(I) when is_integer(I) ->
    to_binary(integer_to_list(I));
to_binary(F) when is_float(F) ->
    to_binary(io_lib:format("~w", [F]));
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.

is_file_exists(FilePath) ->
    case file:read_file_info(FilePath) of
        {ok, _}    -> true;
        {error, _} -> false
    end.
