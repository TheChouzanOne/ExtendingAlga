# Breadth First Search: the forest returns

Hello :)

Last [post](https://github.com/TheChouzanOne/ExtendingAlga/blob/master/Blog/bfs.md) I decided to create a BFS algorithm for alga. However it turns out that some behavior is still missing. By talking with Andrey Mohkov, we came to the conclusion that BFS should return a `Data.Tree` type instead of an `AdjacencyMap a`. This turned out to be a problem because `Data.Tree` does not have a way to represent empty trees, which was the return value when we called `bfsTree` with a non existent vertex.

Also, alga's current API returns a Forest for DFS and not a tree, for the same problem. That's why we are going to modify the code to now mimic alga's API for DFS. In other words, we are going to create a `bfsForest` function.

As a final but important note, as Andrey pointed out, `bfsTree` is a misleading name for the function given that it returns an `AdjacencyMap a` type, so I decided to rename it to `bfsTreeAdjacencyMap`. Quite a long name, but it should be hidden from the API user.

## Algorithm idea

Our ultimate goal is to build `bfsForest`, and its always easir to build on top of what we already have. So the general idea will be to take a graph `g` and compute its BFS forest with a function called `bfsForestAdjacencyMap`. As its name suggests, its return type is `[AdjacencyMap a]`. Then, we could have a new function called `bfsTree` that returns an actual `Data.Tree` type, given a vertex. This function should only be called with valid vertices, so it will be for internal use only. This way, `bfsForest` could map over the result of `bfsForestAdjacencyMap` using the function `bfsTree` somehow. 

It might sound confusing now, but it makes sense. I do have some concerns about this approach. Building on top of what we have might not be a good idea performance-wise, as `bfsTreeAdjacencyMap` is not the best way to compute a tree. But remember from last post that performance is not a priority (yet).

Lets start building the functions one by one.

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

Eventually, by recursively inducing the graph this way, it will reduce into an empty graph, and `bfsForest` will return an empty list `[]`, finishing the algorithm. But how do we compute `headTree`? We can make use of our old friend `bfsTree`. It requires a vertex to be called, so need to retrieve any vertex from `g` beforehand. This can be done with `(head . vertexList) g` easily. Therefore, the final function is as follows:

```Haskell
bfsForest :: Ord a => AdjacencyMap a -> [Tree a]
bfsForest g
    | isEmpty g = []
    | otherwise = headTree : bfsForest (induce (\x -> not (elem x (flatten headTree))) g)
        where headTree = bfsTree ((head . vertexList) g) g
```

MORE DETAIL HERE.

