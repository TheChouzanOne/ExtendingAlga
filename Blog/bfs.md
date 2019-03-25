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
This is just an adjacency map passed into the function. There is not much science here?

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

`Map` and `Set` are imported as qualified so that no conflict exists.

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

I call `bfsTreeUtil` and give it an initial queue with `s` as its initial vertex. Also, I've already seen the root vertex, so I pass it to the `seen` Set. `Set.singleton s` creates a set consisting of the element `s`. Finally, I pass the graph.





ADD THIS NOTE: There is one problem. What if the root node doesn't exist? The function will return `vertex 4` for example, even if `4` is not a node in the graph, which defeats the purpose of typesafe implementations. (MIGHT FIX IT JUST IN THE INITIAL FUNCTION BY GUARDING OR MONADING IT)
