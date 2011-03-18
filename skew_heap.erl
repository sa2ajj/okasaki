-module(skew_heap).

-export([new/0, empty/1, insert/2, merge/2, find_min/1, delete_min/1]).

-record(skew_heap_node, {
    rank,
    root,
    aux = [],
    children = []
}).

new() ->
    [].

empty(X) ->
    X == [].

node_link(#skew_heap_node{root=X1}=T1, #skew_heap_node{root=X2}=T2) ->
    case X1 =< X2 of
        true ->
            T1#skew_heap_node{rank=T1#skew_heap_node.rank + 1, children=[T2 | T1#skew_heap_node.children]};

        false ->
            T2#skew_heap_node{rank=T1#skew_heap_node.rank + 1, children=[T1 | T2#skew_heap_node.children]}
    end.

skew_link(X, T1, T2) ->
    Tempo = node_link(T1, T2),

    case X =< Tempo#skew_heap_node.root of
        true ->
            Tempo#skew_heap_node{root=X, aux=[Tempo#skew_heap_node.root | Tempo#skew_heap_node.aux]};

        false ->
            Tempo#skew_heap_node{aux=[X | Tempo#skew_heap_node.aux]}
    end.

ins_tree(T, []) ->
    [T];
ins_tree(T1, [T2 | Ts]) ->
    case T1#skew_heap_node.rank < T2#skew_heap_node.rank of
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
    case T1#skew_heap_node.rank - T2#skew_heap_node.rank of
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
    case T1#skew_heap_node.rank == T2#skew_heap_node.rank of
        true ->
            [skew_link(X, T1, T2) | Rest];

        false ->
            [#skew_heap_node{rank=0, root=X} | Ts]
    end;
insert(X, Ts) ->
    [#skew_heap_node{rank=0, root=X} | Ts].

merge(Ts1, Ts2) ->
    merge_trees(normalize(Ts1), normalize(Ts2)).

find_min([]) ->
    throw(empty);
find_min([T]) ->
    T#skew_heap_node.root;
find_min([T | Rest]) ->
    X = T#skew_heap_node.root,
    Y = find_min(Rest),
    case X =< Y of
        true ->
            X;

        false ->
            Y
    end.

insert_all([], Ts) ->
    Ts;
insert_all([Elem | Rest], Ts) ->
    insert_all(Rest, insert(Elem, Ts)).

get_min([T]) ->
    {T, []};
get_min([T | Ts]) ->
    {T0, Ts0} = get_min(Ts),
    case T#skew_heap_node.root =< T0#skew_heap_node.root of
        true ->
            {T, Ts};

        false ->
            {T0, [T | Ts0]}
    end.

delete_min([]) ->
    throw(empty);
delete_min(Ts) ->
    {#skew_heap_node{aux=Xs, children=C}, Ts0} = get_min(Ts),
    insert_all(Xs, merge_trees(lists:reverse(C), normalize(Ts0))).
