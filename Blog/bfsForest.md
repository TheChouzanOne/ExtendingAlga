# Breadth First Search: the forest returns

Hello :)

Last [post](https://github.com/TheChouzanOne/ExtendingAlga/blob/master/Blog/bfs.md) I decided to create a BFS algorithm for alga. However it turns out that some behavior is still missing. By talking with Andrey Mohkov, we came to the conclusion that BFS should return a `Data.Tree` type instead of an `AdjacencyMap a`. This turned out to be a problem because `Data.Tree` does not have a way to represent empty trees, which was the return value when we called `bfsTree` with a non existent vertex.

Also, alga's current API returns a Forest for DFS and not a tree, due to the same problem. That's why we are going to modify the code to now mimic alga's API for DFS. In other words, we are going to create a `bfsForest` function.

As a final but important note, as Andrey pointed out, `bfsTree` is a misleading name for the function given that it returns an `AdjacencyMap a` type, so I decided to rename it to `bfsTreeAdjacencyMap`. Quite a long name, but it should be hidden from the API user.

## Algorithm idea

First, we need a function to return a `Data.Tree a` type. It will be called `bfsTree` (which now makes sense) and will work the same as `bfsTreeAdjacencyMap` but instead return a `Tree a`. Simple.

Now, we need `bfsForest`. As a forest is just a list of trees `[Tree a]`, we can make use of list construction. Build a `Tree a` given any vertex of the graph using `bfsTree`, add it as a head of the list, and append it to a `Forest a`. This can be done recursively by computing `bfsForest` on the graph after removing the first tree's vertices. This will become clearer later, but first, lets build `bfsTree`.

### bfsTree

Similar to `bfsTreeAdjacencyMap` it type signature should be as follows:

```Haskell
bfsTree :: Ord a => a -> AdjacencyMap a -> Tree a
```

Now, this gets a little tricky because we need to learn `Data.Tree`'s API. It has a built-in function called `unfoldTree` that takes a function `f :: b -> (a, [b])` and a type `b`. What this does is to build a `Tree b` with `rootLabel = b` and `subForest=[]`, recursively calling `f` on the root labels of the subforest generated. It is weird, but helps to quickly build trees. For example `unfoldTree (\x -> if x==0 then (x,[]) else (x,[x-1])) 4` generates `4 -> 3 -> 2 -> 1 -> 0`.

Therefore, `bfsTree` reduces to a call to `unfoldTree f s` where the `f` should return the `neighbors` of a vertex and `s` is just the starting vertex. Therefore we can partially define it as follows:

```Haskell
bfsTree :: Ord a => a -> AdjacencyMap a -> Tree a
bfsTree s g = unfoldTree neighbors s
    where neighbors ...
```

What should `neighbors` be? Remember that it is a function that takes a vertex `a` and should return `(a,[b])`, where `[b]` is just the list of vertices adjacent to `a`, computed via the function `postSet`. But, should we take the neighbors relative to the graph `g`? No!, we should only take the neighbors resulting from calling `bfsTreeAdjacencyTree` on the starting node `s`, as that's the graph that matters. In code this looks like this:

```Haskell
bfsTree :: Ord a => a -> AdjacencyMap a -> Tree a
bfsTree s g = unfoldTree neighbors s
    where neighbors b = (b, Set.toAscList . postSet b $ bfs)
          bfs = bfsTreeAdjacencyMap s g
```

Remember that `postSet` returns a `Set`, so we need to transform it into a list with `Set.toAscList`. The function is now complete and I believe it is pretty clean.

### bfsForest

Given the background from last sections, we can deduce its type signature as follows:

```Haskell
bfsForest :: Ord a => AdjacencyMap a -> [Tree a]
```
As it is a list, we could do list construction (`x:xs`) as the main building method. First thing to consider is that if the input graph is empty, then an empty list should be returned, which can be done via guard clauses:

```Haskell
bfsForest :: Ord a => AdjacencyMap a -> [Tree a]
bfsForest g
    | isEmpty g = []
    | otherwise = ... 
```

So we want to iterate over `g`'s vertices and build a tree from every one of them, but if one was already included in another tree, ignore it. I believe it might be easier to assume that we already have the first tree, lets call it `headTree`. If `bfsForest` computes the forest of a given graph, and we already have a `headTree`, then we can just call this algorithm again with `headTree`'s vertices removed, and use list construction. This is what I mean:

```Haskell
bfsForest :: Ord a => AdjacencyMap a -> [Tree a]
bfsForest g
    | isEmpty g = []
    | otherwise = headTree : bfsForest (induce (\x -> not (elem x (flatten headTree))) g)
```

We recursively call `bfsForest` but on a different graph.  `induce :: (a -> Bool) -> Graph a -> Graph a` helps us generate a new graph given `g`, by just taking all the vertices that return a `true` value in the function, automatically removing every vertex that doesn't and its correspongind edges. Given this, we just pass the function `\x -> not (elem x (flatten headTree))`. `flatten` is a function from `Data.Tree` that takes all the vertices from a `Tree a` and return them as a list `[a]`. Therefore, in a more natural language this is read like: for every vertex `x`, if `x` is not a vertex of `headTree`, then return `true`.

Eventually, by recursively inducing the graph this way, it will reduce into an empty graph, and `bfsForest` will return an empty list `[]`, finishing the algorithm. But how do we compute `headTree`? We can make use of our previous friend `bfsTree`. It requires a vertex to be called, so we need to retrieve any vertex from `g` beforehand. This can be done with `(head . vertexList) g` easily. Therefore, the final function is as follows:

```Haskell
bfsForest :: Ord a => AdjacencyMap a -> [Tree a]
bfsForest g
    | isEmpty g = []
    | otherwise = headTree : bfsForest (induce (\x -> not (elem x (flatten headTree))) g)
        where headTree = bfsTree ((head . vertexList) g) g
```

Finally, `bfsForest` is complete and works as intended! It does not look as clean as `bfsTree` sadly, but it works and that fills me with joy :) .

#### Testing bfsForest

To convince you that bfsForest works, here are some examples (taken/based from [alga](https://github.com/snowleopard/alga/blob/master/src/Algebra/Graph/AdjacencyMap/Algorithm.hs)'s DFS documentation).

To show this easier, I will use an already defined function called `forest` which takes a `Forest a` type and converts it into an `AdjacencyMap a` type, as showing `Forest a` gets a little trickier.

```Haskell
bfsForest 'empty'                       == []
forest (bfsForest $ 'edge' 1 1)         == vertex 1
forest (bfsForest $ 'edge' 1 2)         == edge 1 2
forest (bfsForest $ 'edge' 2 1)         == vertices [1,2]
bfsForest $ 1 * (3+5+7) + 3 * (5+4) + (4+3+5+7) * 6 ==  [Node {rootLabel = 1
                                                              , subForest = [Node {rootLabel = 3
                                                                                  , subForest = [ Node {rootLabel = 4
                                                                                                       , subForest = [] }
                                                                                                , Node {rootLabel = 6
                                                                                                       , subForest = [] }]}
                                                                            , Node {rootLabel = 5
                                                                                   , subForest = [] }
                                                                            , Node {rootLabel = 7
                                                                                   , subForest = [] }]}]
```

To make it easier to work around the last example, here is an image of the graph before applying `bfsForest`to it: ![idkWhatToPutHere](https://github.com/TheChouzanOne/ExtendingAlga/blob/master/Blog/img/bfsForest/test.PNG)


## What's next?

I am starting to like iterating over my first BFS implementation, as before (and during) the writing of this post I modified the implementation significantly and even manage to remove a function. So I really want to meet the standards for this to be added to alga. Depending on feedback, I might create functions `bfs` and `bfsForestFrom`, focus on performance or start building weighted graph algorithms. Actually, the latest sounds like the most fun option, but I'll see and keep you updated.

Have a nice week :).

