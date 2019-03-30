# Introduction - First steps

The objective of this blog is to share [alga](https://github.com/snowleopard/alga) and make it easier for newcomers to understand it. I'll do my best as I am a newcomer as well, not only to this library, but to Haskell in general. I first heard about Haskell on the book Seven Languages in Seven Weeks and it has been in my programming languages'to learn' list since. It so happened to be the perfect time to cross it out as one of the projects for [GSoC 2019](https://summerofcode.withgoogle.com/) consists of improving this library by adding some famous algorithms for weighted graphs and developing a typesafe representation for acyclic graphs. Thus, I will focus on the Labelled module of the library (and maybe come up with a new module named *acyclic*).

Also, this is the first time I write a blog so bear with me, the quality of the posts will improve over time.


## Where to start?

Well, the main point of this blog is to share information about alga. There are some good introductions on the algebra behind alga, how to create graphs, why are they safe and so on. The main reference is [this paper](https://github.com/snowleopard/alga-paper) written by the author of alga, Andrey Mokhov, so I do recommend reading it first. Also, from previous GSoC a small [tutorial](https://nobrakal.github.io/alga-tutorial/index.html) has been written by Alexandre Moine.

I can first focus on the structure of the library. There is not much documentation on what each module contains (even though it can be derived from their name), so I will try to give a quick summary after skimming through the code from every single one of them. If there are any mistakes, feel free to contact me and I will correct them ASAP. Also keep in mind that most of the text in every file are comments describing the behavior of the functions and the purpose of the module, which is pretty helpful.

#### Algebra.Graph

This is where the definition for the Graph data type is defined and its instance declarations can be found. A lot of the stuff talked about in the paper can be found here, such as the principal graph constructors (`empty`, `vertex`, `overlay`, `connect`) or common graph constructors like `mesh`, `star`, `clique`, `path`, etc. Also, some rules for the compiler are defined here, rules that I haven't done any research on yet.

#### Algebra.Graph.Class

Here, the type class Graph is defined. There is a name collision between the data type and the type class, but it does not really matter. It just a good thing to be aware of. For every different representation of graphs found in this library (Labelled, AdjacencyMap, Unlabelled, etc.), there are graph instance declarations for them.

#### Algebra.Graph.Fold

Fold is a module that permits the user to transform the graph in many different ways. As the name sugests, it does a lot of folding with graphs. Foldg is one of the main functions in graphs, which allows us to manipulate them in very interesting ways. For example, its use is as follows:

```haskell
foldg e v o c g
```

where e refers to Empty, v to Vertex, o is Overlay, c means Connect and g refers to the graph. If we just subtitute these Types with the parameters, the graph remains the same, but if we instead use `v = connect v v` then the resulting graph will be reflexive (all its vertices contain self loops).

#### Algebra.Graph.NonEmpty

This module defines behavior for graphs known to be non empty. There is another data type called Graph (to dodge any name collision, this module can be imported as qualified and some funcitions have a suffix of '1' e.g. foldg1) without the value Empty.

#### Algebra.Graph.Relation

This is one of the modules that I find easier to understand. It defines the Relation data type the same as the mathematical form discussed in the paper, which makes it more digestable in my opinion. This helps us define relations such as transitive, reflexive or symmetric in a much readable way.

#### Algebra.Graph.AdjacencyMap and Algebra.Graph.AdjacencyIntMap

These modules define the data type AdjacencyMap (**AM**) deriving from the Graph type class. This is the most common form of graphs representation found in any programming language. AdjacencyIntMap is a special module used for graphs whose vertex types are Ints.

These modules are the only ones that support algorithms (**Algebra.Graph.AdjacencyMap.Algorithm**).

From what I've seen, for every type of Graph, AMs are used as an intermediate type to work with algorithms, converting any other Graph type into an AM and applying a function such as ``scc`` or ``dfsForest``.

#### Algebra.Graph.Label

Pretty useful module as it defines different labels to use on labelled graphs. Perhaps the most important label is Distance.

#### Algebra.Graph.Labelled

This module removes the Overlay value from the Graph type, as it is now represented as Connect 0 graph1 graph2 just to give an example. 
Also, its own AdjacencyMap for working with labels is defined, thus, this will be the main module for working with weighted algorithms, so I am sure there will be a special post regarding it in the future.

#### Other modules and concerns

There are a few other modules in the library, but I believe they are not as important as the ones mentioned above, and I doubt they will be used.
There are some things that I don't understand, such as the purpose of the Fold module. Every other graph type has its own definition of foldg and there is no reference to this module. This might mean that it is not such a useful module for the purpose of this blog.

Nevertheless, I hope these brief descriptions are useful and I will be updating them as the library becomes clearer to me.

## Acyclic graphs

It might be a good idea to talk about my first thoughts on how to solve this problem. Just to be clear, these are my first thoughts and they might be wrong. Every DAG can be topologically sorted, so I believe that a typesafe representation should be closely related to the definition of a topological sort:

> Given a graph G = (V,E), for every edge (u,v) ∈ E, u comes before v in the ordering.

How could we use this definition to solve the problem? Well, I don't know anymore. Now that I write it down it doesn't seem practical to *build* a topological sort every time a new graph is created or a vertex is connected, as there are many orderings that satisfy this property, which was my first thought. Maybe, Just Maybe (see what I did there?), every time two graphs are combined, using the Maybe class an Acyclic Graph could be returned or Nothing if it is no longer cyclic. This sounds like a pretty bad idea performance-wise though. 

This problem obviously needs more thought and research and I am sure some actually good ideas will come to me in time. Also stated in the GSoC project idea, there's an [issue](https://github.com/snowleopard/alga/issues/152) regarding `scc` and `topSort`. My goal would be to create a type that solves this problem as well. It has been mentioned that phantom types might work, so I will need to check that out as well.

## Implementing Kruskal, Dijkstra and Bellmand-Ford (optionally network flow algorithms)

This sounds like a pretty fun project. I've participated in the ICPC Regional Finals at Mexico and I love implementing algorithms, although I've never done analysis on functional programs. I guess it's the same as recursive function calls in imperative languages.

For Kruskal, I don't believe it should be a hard task to complete if I'm being honest. I haven't had time to try some ideas, but the algorithm should be pretty similar as the most known one: sort edges by weight and take one by one using disjoint sets to know which vertices has been connected until there is only one set remaining and end up with a MSP. I might be thinking too lightly about it as I haven't seen how to implement disjoint sets on haskell (or if the Data.DSet is good enough), but I feel confident about this one.

Regarding the other algorithms, they probably will be harder to implement, but no worries there as *Introduction to Algorithms* by Thomas H. Cormen will be my inspiration to solve these problems. As AdjacencyMap being the main data type to use, it will be easier to implement these. 

This is the part of the project I am more excited about. I've also seen that the work done in the previous year of GSoC is a [benchmark suite](https://github.com/haskell-perf/graphs) to compare algorithms from different libraries (fgl, containers, hash-graph and alga). It might be a good idea to see how to use this benchmark suite and compare my results to other libraries that have implemented these algorithms.

## What's next?

Well, this is the end of my first post. It was basically a brainstorm of ideas. The next step is to actually get my hands dirty and start coding some graphs with the library. I've noticed that there's no `bfs` algorithm implemented, while `dfs` is and I don't know why. I will try to implement `bfs` on the next blog to get the hang of the library and haskell before diving into the labelled world.

Any comments are welcome and much appriciated :)



