-module(exodm_plugin_lib).

-export([data_to_json/3]).

-include_lib("lager/include/log.hrl").

data_to_json(Spec, Env, Data) ->
    ?debug("data_to_json(~p, ~p, ~p)~n", [Spec, Env, Data]),
    case find_leaf(<<"rpc-status-string">>, Spec) of
        false ->
            yang_json:data_to_json(Spec, Env, Data);
        _Leaf ->
            case keyfind(<<"rpc-status-string">>, Data) of
                false ->
                    case keyfind(<<"rpc-status">>, Data) of
                        false ->
                            yang_json:data_to_json(Spec, Env, Data);
                        Status ->
                            case enum_descr(find_leaf(<<"rpc-status">>, Spec),
                                            to_binary(element(2, Status))) of
                                false ->
                                    yang_json:data_to_json(Spec, Env, Data);
                                Descr ->
                                    yang_json:data_to_json(
                                      Spec, Env,
                                      [{<<"rpc-status-string">>, Descr}|Data])
                            end
                    end;
                _ ->
                    yang_json:data_to_json(Spec, Env, Data)
            end
    end.

enum_descr(false, _) -> false;
enum_descr({leaf, _, _, I}, V) ->
    case lists:keyfind(type, 1, I) of
        {_, _, <<"enumeration">>, I1} ->
            enum_descr_(I1, V);
        _ ->
            false
    end.

%% Assume rpc-status can be either the numeric value or the description.
enum_descr_([{enum,_,V,I}|_], V) ->
    case lists:keyfind(description,1,I) of
        {_, _, Descr, _} -> Descr;
        false -> V
    end;
enum_descr_([{enum,_,D,I}|T], V) ->
    case lists:keyfind(value, 1, I) of
        {_, _, V, _} ->
            case lists:keyfind(description,1,I) of
                {_, _, Descr, _} -> Descr;
                false -> D
            end;
        _ ->
            enum_descr_(T, V)
    end;
enum_descr_([_|T], V) ->
    enum_descr_(T, V);
enum_descr_([], _) ->
    false.

find_leaf(K, [{leaf,_,K,_} = L|_]) -> L;
find_leaf(K, [_|T]) -> find_leaf(K, T);
find_leaf(_, []) -> false.

keyfind(A, [H|T]) when is_tuple(H) ->
    K = element(1, H),
    case comp(A,K) of
        true ->
            H;
        false ->
            keyfind(A, T)
    end;
keyfind(_, []) ->
    false.

to_binary(A) when is_atom(A) ->
    atom_to_binary(A, latin1);
to_binary(I) when is_integer(I) ->
    list_to_binary(integer_to_list(I));
to_binary(L) when is_list(L) ->
    list_to_binary(L);
to_binary(B) when is_binary(B) ->
    B.

comp(A, A) -> true;
comp(A, B) when is_binary(A), is_list(B) ->
    binary_to_list(A) == B;
comp(A, B) when is_binary(A), is_atom(B) ->
    A == atom_to_binary(B, latin1);
comp(_, _) ->
    false.
