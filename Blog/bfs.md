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
