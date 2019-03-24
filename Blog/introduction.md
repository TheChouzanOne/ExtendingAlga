# Introduction

The objective of these posts are to share [alga](https://github.com/snowleopard/alga) and make it easier for newcomers to understand it. I'll do my best as I am a newcomer as well, not only on this library but haskell in general. I first heard about it on Seven Languages in Seven Weeks and has been in my interest languages list for a long time. Now was the proper time to take it out of there as one of the projects for [GSoC 2019](https://summerofcode.withgoogle.com/) consists of improving this library by adding some famous algorithms for weighted graphs and coming up with a typesafe representation for acyclic graphs. Thus, I will focus on the Labelled module of the library (and maybe come up with a new module called acyclic?).

Also, this is the first time I write a blog so, bear with me. The quality of posts will get better over time.


## Where to start?

Well, the main point of this blog is to share alga. There are some good introductions on the algebra behind alga, how to create graphs, why are they safe and so on. The main reference is [this paper](https://dl.acm.org/authorize.cfm?key=N46678) written by the author of alga, Andrey Mokhov, so I do recommend reading it first. Also, from previous years of GSoC a small [tutorial](https://nobrakal.github.io/alga-tutorial/index.html) has been written by Alexandre Moine.

I can first focus on the structure of the library. There is not much documentation on what each module contains (even though it can be derived from its name), so I will try to give a quick summary after skimming the code from every single one of them. If there are any mistakes, feel free to contact me and I will correct them asap. Also keep in mind that most of the text in every file are comments describing the behavior of the functions and the purpose of the module, which is pretty helpful.

#### Algebra.Graph

This is where the definition for the Graph data type is defined and its instance declarations can be found. A lot of the stuff from the paper can be found here, such as the principal graph constructors (`empty`, `vertex`, `overlay`, `connect`) or common graph constructors like `mesh`, `star`, `clique`, `path`, etc. Also, some rules for the compiler are defined here, rules that I haven't done any research on yet.

#### Algebra.Graph.Class

Here, the type class Graph is defined. There is a name collision between the data type and the type class, but it does not really matter. It just a good thing to be aware of. For every different representation for graphs found in this library (Labelled, AdjacencyMap, Unlabelled, etc.), there are graph instance declarations for them.

#### Algebra.Graph.Fold

Fold is a module that permits the user to transform the graph in many different ways. As the name sugests, it does a lot of folding with graphs. Foldg is one of the main functions on graphs, which allows us to manipulate them in very interesting ways. For example, its use is as follows:

```haskell
foldg e v o c g
```

where e refers to Empty, v to Vertex, o is Overlay, c means Connect and g refers to the graph. If we just subtitute these Types on the parameters, the graph remains the same, but if we say `v = connect v v` then the resulting graph will be reflexive (all its vertices contain self loops).

#### Algebra.Graph.NonEmpty

This module defines behavior for graphs known to be non empty. There is another data type called Graph (to dodge any name collision, this module can be imported as qualified and some funcitions have a suffix of '1' e.g. foldg1) without the value Empty.

#### Algebra.Graph.Relation

This is one of the modules that I find easier to understand. It defines the Relation data type the same as the mathematical form discussed in the paper, which makes it more digestable in my opinion. This helps to define relations such as transitive, reflexive or symmetric in a much readable way.

#### Algebra.Graph.AdjacencyMap and Algebra.Graph.AdjacencyIntMap

These modules define the data type AdjacencyMap (**AM**) deriving from the Graph type class. This is the most common form of graphs representation found in any programming language. AdjacencyIntMap is a special module used for graphs whose vertex types are Ints.

These modules are the only ones that support algorithms (**Algebra.Graph.AdjacencyMap.Algorithm**).

From what I've seen, for every type of Graph, AMs are used as an intermediate type to work with algorithms, converting any other Graph type into an AM and applying a function such as ``scc`` or ``dfsForest``.

#### Algebra.Graph.Label

Pretty useful module as it defines different labels to use on labelled graphs. Perhaps the most important label is Distance.

#### Algebra.Graph.Labelled

This module removes the Overlay value from the Graph type, as it is now represented as Connect 0 graph1 graph2 for example. 
Also, its own AdjacencyMap for working with labels is defined, thus, this will be the main module for working with weighted algorithms so I am sure there will be a special post for it in the future.

#### Other modules and concerns

There are a few other modules in the library, but I believe they are not as important as the ones mentioned above, and I think they will not be used.
There are some things that I don't understand, such as the purpose of the Fold module. Every other graph type has its own definition of foldg and there is no reference to this module. This might mean that it is not such a useful module for the purpose of this blog.

### Name this

Well, the first task is to come up with a typesafe representation for **acyclic graphs**, so it might be a good start to talk about my first thoughts on these. 

