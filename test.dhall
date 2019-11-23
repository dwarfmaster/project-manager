
let Tree : Type = forall (T : Type) -> (Natural -> T) -> (T -> T -> T) -> T
    let leaf : Natural -> Tree = \(i : Natural) -> \(T : Type) -> \(lf : Natural -> T) -> \(nd : T -> T -> T) -> lf i
    let node : Tree -> Tree -> Tree =
        \(t1 : Tree) -> \(t2 : Tree) -> \(T : Type) -> \(lf : Natural -> T) -> \(nd : T -> T -> T) ->
            nd (t1 T lf nd) (t2 T lf nd)
in node (node (leaf 5) (node (leaf 42) (leaf 3))) (leaf 69)

