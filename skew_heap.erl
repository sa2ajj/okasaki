-module(skew_heap).

-export([new/0, empty/1, insert/2, merge/2, find_min/1]).

%% -type skew_heap_tree() :: {skew_heap_node, integer(), term(), [term()], [skew_heap_tree()]}.

new() ->
    [].

empty(X) ->
    X == [].

rank({skew_heap_node, Rank, _, _, _}) ->
    Rank.

root({skew_heap_node, _, Root, _, _}) ->
    Root.

node_link({skew_heap_node, Rank, X1, XS1, C1}=T1, {skew_heap_node, _, X2, XS2, C2}=T2) ->
    case X1 =< X2 of
        true ->
            {skew_heap_node, Rank + 1, X1, XS1, [T2 | C1]};
            
        false ->
            {skew_heap_node, Rank + 1, X2, XS2, [T1 | C2]}
    end.

skew_link(X, T1, T2) ->
    {skew_heap_node, Rank, Y, Ys, C} = node_link(T1, T2),

    case X =< Y of
        true ->
            {skew_heap_node, Rank, X, [Y | Ys], C};
            
        false ->
            {skew_heap_node, Rank, Y, [X | Ys], C}
    end.

ins_tree(T, []) ->
    [T];
ins_tree(T1, [T2 | Ts]) ->
    case rank(T1) < rank(T2) of
        true ->
            [T1 | [T2 | Ts]];
            
        false ->
            ins_tree(node_link(T1, T2), Ts)
    end.

merge_trees(Ts1, []) ->
    Ts1;
merge_trees([], Ts2) ->
    Ts2;
merge_trees([T1 | Ts1], [T2 | Ts2]) ->
    case rank(T1) - rank(T2) of
        X when X < 0 ->
            [T1 | merge_trees(Ts1, [T2 | Ts2])];

        X when X > 0 ->
            [T2 | merge_trees([T1 | Ts1], Ts2)];

        _ ->
            ins_tree(node_link(T1, T2), merge_trees(Ts1, Ts2))
    end.

normalize([]) ->
    [];
normalize([T | Ts]) ->
    ins_tree(T, Ts).

insert(X, Ts=[T1 | [T2 | Rest]]) ->
    case rank(T1) == rank(T2) of
        true ->
            [skew_link(X, T1, T2) | Rest];
            
        false ->
            [{skew_heap_node, 0, X, [], []} | Ts]
    end;
insert(X, Ts) ->
    [{skew_heap_node, 0, X, [], []} | Ts].

merge(Ts1, Ts2) ->
    merge_trees(normalize(Ts1), normalize(Ts2)).

find_min([T]) ->
    root(T);
find_min([T | Rest]) ->
    X = root(T),
    Y = find_min(Rest),
    case X =< Y of
        true ->
            X;

        false ->
            Y
    end.
