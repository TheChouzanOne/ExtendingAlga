# Breadth First Search (BFS)

In this post, I will try to implement the famous BFS algorithm on alga's AdjacencyMap (**AM**) data type. Maybe I am blind, but I did not find it in the `Algebra.Graph.AdjacencyMap.Algorithm` module, so I thought it would be a good idea to do it myself (at least a light version) to practice and get used to the structure.

Maybe I was too beaten yesterday to think properly, but I had a bad time trying to get my head around the types used. Eventually, after looking at how some algorithms are implemented on alga's library I discovered how I might be able to do it.

## Algorithm idea

So, it is a pretty known algorithm so I won't spend much time here. If you want to get a good idea you can check [this link](https://www.geeksforgeeks.org/breadth-first-search-or-bfs-for-a-graph/). I won't compute the node distance from a  vertex to all the others, but instead, will focus on computing the BFS tree generated form a source node.

The link shows an imperative algorithm, but we are working in a purely functional language, so we need to think recursively. What do we need?
  * A graph.
  * A queue to add vertices.
  * Some way to know which vertices has been visited/seen.

### How will I represent these elements?

#### The graph
This is just an adjacency map passed into the function. There is not much science here I believe.

#### Queue
I did not find an official queue for haskell (probably because of its mutable nature) so I decided to implement it just as a list, where dequeue will work as list deconstruction (`x:xs`) and enqueueing (?) elements to the list will work as a concatenation (`++`). I know this last part will be expensive, but I did not want to focus on efficiency yet.

#### Seen vertices
This will be implemented via the Data.Set data structure from Haskell, as it comes with pretty useful functions such as `union` and `difference`, and functions to convert Sets to Lists (`toList`,`toAscList`,`toDecList`).

## What's an AdjacencyMap (AM)?
This data structure is defined in the `Algebra.Graph.AdjacencyMap.Internal` module, and is defined as a map of vertices as keys and sets of vertices as values. It is basically a hash table that maps a vertex to its neighbors (edges pointing outwards). The function `adjacencyMap` converts an `AdjacencyMap a` into a `Map a (Set a)` type, which helps to access its contents.

## Which modules should I import?
Well, according to last sections, we need to import Sets, Maps and AdjacencyMaps. As there might be conflicts between modules (such as `map` functions defined at Sets or Maps), the import lines should look like this:

```Haskell
import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.AdjacencyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
```
There is no conflict between both AdjacencyMap modules. One thing to notice is that `.Internal` contains the type `AM`, which needs to be used to convert a `Map a (Set a)` back into an `AdjacencyMap a`. The other one exports useful functions that will be explained later.

`Map` and `Set` are imported as qualified so that no conflict exists. I used `Strict` for `Map` as that's how its done in the library, but I really do not know why. After a quick search, it seems that `Strict` makes `Int` maps more efficient, so I guess that's good.

## How should BFS function look like?

Here starts the fun part. This algorithm should return an AM consisting of the BFS tree that results after choosing a root vertex. So the type should look like:

```Haskell
bfsTree :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
```
First, `a` needs to be an instance of the `Ord` type class because many `Set` functions require its element to be like that (it is implemented using a balanced tree, so it makes sense). Then, the functions takes a vertex and a graph and returns a graph. Pretty straight forward. This is a friendly function for the user, but to use recursion and be able to share information from recursive call to recursive call, it needs to support a queue and a set of seen vertices, so a support function is defined as

```Haskell
bfsTreeUtil :: Ord a => [a] -> Set.Set a -> AdjacencyMap a -> AdjacencyMap a
```
Again, `a` needs to be an `Ord` instance. Now, this function takes a queue of vertices (`[a]`), a set of seen items `Set.Set a`, a graph and returns a graph. Here is where the fun part will happen. This means that `bfsTree` implementation should be easy:

```Haskell
bfsTree :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
bfsTree s g = bfsTreeUtil [s] (Set.singleton s) g
```

I call `bfsTreeUtil` and give it an initial queue with `s` as its initial vertex. Also, I've already seen the root vertex, so I pass it to the `seen` Set. `Set.singleton s` creates a set consisting of the element `s`. Finally, I pass the graph to the function.

## But how will bfsTreeUtil work exactly?

Recursively, of course. My idea was to take the first element of the queue: the vertex, get all its neighbors that haven't been seen before, connect the vertex to these neighbors, create the new set of seen vertices, add new vertices to the queue and overlay the created graph to the result of recursively calling the function on the resulting elements and the tail of the queue. It will be much clearer once we see the code.

### The pieces

#### Neighbors

So we one thing we need is to get the neighbors of a vertex. This can be done by using the function `postSet` from the `Algebra.Graph.AdjacencyMap` module. It basically returns the associated value from the given key, as `AdjacencyMap` is just a `Map`.

#### Vertices to connect the "root" vertex to.

This is core to BFS, so we'll need to implement this one. There is one key point: if we have seen a vertex before and it is connected to the current one, we need to ignore it. As `AdjacencyMap`s use `Set`s, we will return a `Set`. We'll call it `vSet`. I will ignore the type signature as we will see later:

```Haskell
vSet neighbors seen = Set.difference neighbors seen
```
The solution is simple. We assume we already have the neighbors of some vertex and some way to tell which vertices has been seen before. Both are `Set`s, so we just need to compute the difference of these. Note that neighbors should come first, as otherwise we would get only vertices that we have seen before.

#### Adding vertices to the "seen" Set
We need to know which vertices will be marked as seen after "iterating" on a node, so we need a `newSeen` function:

```Haskell
newSeen seen neighbors = Set.union seen neighbors
```

Again, we assume we know the neighbors of the current vertex and which vertices has been seen so far. We just need to compute the union of these two sets. Makes sense, right?

#### And finally, enqueueing vertices
As I promised, this will be done with lists. Assume we have a current queue and know which vertices need to be enqueued. Lets call them `qv` and `vSet` respectively. The q in `qv` stands for queue for readibility and `vSet` is basically the same as the `vSet` functions defined earlier. Its arguments are omitted. It's okay, I promise it will be clearer later.

```Haskell
newQueue qv vSet = qv ++ (Set.toAscList vSet)
```

We just take the current queue `qv` which is a list and concatenate to the list of `vSet`. As the name says, `vSet` is a `Set`, so we need to transform it to a `List`. This can be done using `Set.toAscList`, which takes a set and transform its elements into an ascending order `List`. This is done so "smaller" vertices are prioritized.

### Finally, it all comes together

Now we can start implementing the logic behind the magical `bfsTreeUtil` function. It is recursive so we need a base case. According to the usual algorithm, it ends once the queue is empty, so the base case looks like

```Haskell
bfsTreeUtil :: Ord a => [a] -> Set.Set a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeUtil [] _ _ = empty
```

We use pattern matching, if the queue received is empty, we just return an empty graph. What if the queue is not empty? Well, we need to dequeue an element. This can be done with the `:` operator to take the head off the queue. So the queue argument will look like `(v:qv)` (You remember `qv`, right?). To make it more readable, we can add an *as-pattern*: `queue@(v:qv)`. We won't use it, and might make the algorithm a little slower, but makes it clear that this argument is acting as a queue.

Now, the set of known vertices will just be called `seen` and the graph `g`. This means that the start of the function would look like this (I am keeping the base case as reference):


```Haskell
bfsTreeUtil [] _ _ = empty
bfsTreeUtil queue@(v:qv) seen g = ...
```

What's next? Well, first, the current vertex is clearly `v`, as we are "dequeueing" it from the list. Now I said we were going to overlay the connection of this vertex to its unseen neighbors and the recursive call of the function. Which function helped us to get the unseen neighbors... hmm... Oh, that's it!`vSet`! Now, it might be a good idea to explain what does overlay do. It just takes two graphs as inputs and overlaps them. Read the paper for an in-depth explanation. The key part here is... "it takes two graphs as inputs" so we need to create an `AdjacencyMap` from what we have. Recall that the type derived from `adjacencyMap` is `Map a (Set a)`, so we need to build this structure from what we know. This can be done easily by `Map.singleton v vSet`. This graph contains only one element in its map, we we can use `singleton` for that. We associate the `vSet` as the value of key `v`. Now, we convert it to an `AdjacencyMap` by calling the type constructor `AM` like so: `AM $ Map.singleton v vSet`. Now we can easily derive the next part of `bfsTreeUtil`:

```Haskell
bfsTreeUtil [] _ _ = empty
bfsTreeUtil queue@(v:qv) seen g = overlay (AM $ Map.singleton v vSet) ...
```

but how does it know what arguments do vSet have? This is where `where` comes in! We can declare the function as follows:

```Haskell
bfsTreeUtil [] _ _ = empty
bfsTreeUtil queue@(v:qv) seen g = overlay (AM $ Map.singleton v vSet) ...
    where
        neighbors = postSet v g
        vSet = Set.difference neighbors seen
```
Now we are starting to use the pieces we built earlier. These functions do not need arguments as they are already found at the start of `bfsTreeUtil`.

`overlay` still needs its second argument, which should be a graph. We will pass it recursively, as the result of calling `bfsTreeUtil` on the resulting `queue` and set of `seen` vertices. So it looks like:

```Haskell
bfsTreeUtil [] _ _ = empty
bfsTreeUtil queue@(v:qv) seen g = overlay (AM $ Map.singleton v vSet) (bfsTreeUtil newQueue newSeen g)
    where
        neighbors = postSet v g
        vSet = Set.difference neighbors seen
```

But what does `newQueue` and `newSeen` mean? Well, we need more `where` clauses:
 
```Haskell
bfsTreeUtil [] _ _ = empty
bfsTreeUtil queue@(v:qv) seen g = overlay (AM $ Map.singleton v vSet) (bfsTreeUtil newQueue newSeen g)
    where
        neighbors = postSet v g
        vSet = Set.difference neighbors seen
        newSeen = Set.union seen neighbors
        newQueue = qv ++ (Set.toAscList vSet)  
```

We just add the final pieces that we built before. That's it! We have finished implementing `bfsTreeUtil`. Try it for yourself! If you have the graph `g = 1*2 + 1*3 + 1*4 + 2*4 + 3*5 + 4*6 + 5*6` (I'll let you figure out how this one looks, remember to read the [paper](https://github.com/snowleopard/alga-paper)), then 

`bfsTree 1 g = edges [(1,2),(1,3),(1,4),(3,5),(4,6)]`

If you think about it, this is correct! I haven't tested it througly, but it has worked fine for all my tests. There is just one problem.

## The problem

What if the root node doesn't exist in the graph? By the definition of `bfsTree`, we always pass the initial vertex as the first element of the queue. But if it doesn't exist in the graph, we get a wrong answer. Remember `g` from last section? Well, if we call `bfsTree 7 g` we get `vertex 7`. But how can BFS return a vertex that doesn't exist in the original graph? This is not a typesafe definition and defeats the purpose of alga. 

Hopefully, there is an easy fix and does not involve changing the algorithm, but the initial call at `bfsTree`. What if we first check if the vertex exists at `g` and then take a decision? We could return `Nothing` if it doesn't exist, or an `empty` graph as a result, but after checking the behaviour of `postSet`, it returns `Set.empty` if we call it on a node that doesn't exist, so lets keep it consistent and return `empty` as well.

The fix is to just wrap the `bfsTree` function defition as follows.

```Haskell
bfsTree s g = if (hasVertex s g) then bfsTreeUtil [s] (Set.singleton s) g else empty
```

`hasVertex` is a built-in function from `Algebra.Graph.AdjacencyMap` so we just use it and if its true, we proceed as normal, otherwise we just return an `empty` graph. Easy!

So, the final code is as follows:

```Haskell
module BFS (
    --Algorithms
    bfsTree
) where

import Algebra.Graph.AdjacencyMap.Internal
import Algebra.Graph.AdjacencyMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

bfsTree :: Ord a => a -> AdjacencyMap a -> AdjacencyMap a
bfsTree s g = if (hasVertex s g) then bfsTreeUtil [s] (Set.singleton s) g else empty

bfsTreeUtil :: Ord a => [a] -> Set.Set a -> AdjacencyMap a -> AdjacencyMap a
bfsTreeUtil [] _ _ = empty
bfsTreeUtil queue@(v:qv) seen g = overlay (AM $ Map.singleton v vSet) (bfsTreeUtil newQueue newSeen g)
    where
        neighbors = postSet v g
        vSet = Set.difference neighbors seen
        newSeen = Set.union seen neighbors
        newQueue = qv ++ (Set.toAscList vSet)
```

We added a module declaration so this function can be exported easily and hide `bfsTreeUtil` from the API.

## Final comments

I really enjoyed writing this function. I do believe it is elegant and pretty short compared to imperative languages. It might not be the most efficient or most elegant way to write it, and might not be right to compare it to imperative programming, but I think it's hard to refute its shortness.

I am unsure if this post is unnecesarily large, but I wanted to explain everything to detail. Again, I want to try to make it as friendly as possible to newcomers like me. Remember that any comments are welcome.

Also, I am not sure what will come after this post, but if this implementation is good enough, I believe it would be a good idea to benchmark it, or improve it if necessary.

That's it for me, have a great day and thank you for taking the time to read this!


