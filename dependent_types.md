# Dependent Types in Haskell

Haskell currently does not support dependent types natively but there are certain extensions that help us to get there.

It is not as elegant as it would be if we had build in support for this but Richard Eisenberg is busy at work doing a great
work to make this happen. 

We will go trough different steps in explaining dependent types in Haskell:

* Terms vs. Types
* Type level data
* Lambda Cube
* Local assumptions
* Generic 
* Sigma and Pi

## Terms vs. Types
Difference between the terms and types can be looked at in few different ways. 

First one would be that if you look at type annotation, anything to the left of the `::` is considered a term while anything to the right of it is the type.

Another way of looking at this could be with regards to `=` sign in function definition but this time it is other way around - anything to the left of `=` is a type and anything to the right a type. 

We can also say something that applies to standard Haskell (without extensions) and that is that terms are present at runtime while types get erased at runtime. This statement is not true when it comes to dependent types but it serves well in the path of our understanding of the topic.

So far so good right ?

## Type level data
We can say for a type that it has a possible set of values that correspond to it. So `Void` is a type with zero inhabitants - empty set , `Unit` has a single element set (`()`) , `Bool` has two element set (`True` and `False`) and so on. Here is a small example of this

```
data Void 
data Unit = U
```

In contrast to this types can also have a type level data.
