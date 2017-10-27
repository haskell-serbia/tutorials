# Dependent Types in Haskell

Haskell currently does not support dependent types natively but there are certain extensions that help us to get there.

It is not as elegant as it would be if we had build in support for this but Richard Eisenberg is busy doing a great
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

First one would be that if you look at type annotation:
```
term :: type :: kind
```
You can ignore kinds for now, they are to type what type is to terms - so like type of the type constructor. This sounds complicated but I promise it is not, just think of them as _types one level up_. You can read about kinds [here](https://wiki.haskell.org/Kind)

Another way of looking at this could be with regards to `=` sign in function implementation but this time it is other way around. Anything to the left of `=` are types and when we are to the right of `=` we are in term land (with exceptions of  `::` type annotations and `@` `TypeApplications` extension).

We can also say something that applies to standard Haskell (without extensions) and that is that terms are present at runtime while types get erased at runtime. This statement is not true when it comes to dependent types but it serves well in the path of our understanding of the topic.

So far so good right ?

## Type level data
We can say for a type that it has a possible set of values that correspond to it. So `Void` is a type with zero inhabitants - empty set , `Unit` has a single element set (`U`) , `Bool` has two element set (`True` and `False`) and so on. Here is a small example of this

```
data Void 
data Unit = U
data Bool = True | False
```

In contrast to this types can also have a _type level data_. That is the data that lives on the type level and does not have associated set of inhabitants. We can use type level data to provide extra safety to our programs since that allows us to _describe_ how our types should be constructed thus eliminating more runtime errors. 

Here is a small example from `GHC` manual that provides a way of defining sort of an accessor with convenient type level _String_ (`Symbol`) 
```
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE FlexibleInstances       #-}
module Main where

import GHC.TypeLits

data Label (l :: Symbol) = Get
data Point = Point Int Int deriving Show

class Has a l b | a l -> b where
  from :: a -> Label l -> b

instance Has Point "x" Int
    where from (Point x _) _ = x

instance Has Point "y" Int
    where from (Point _ y) _ = y

example :: Int
example = from (Point 1 2) (Get :: Label "x")

```
So type level data is something that lives on the level of types but is not a type.

## Lambda Cube

[Lambda Cube](https://en.wikipedia.org/wiki/Lambda_cube) is the term from Type Theory and Math Logic. It builds on top of lambda calculus defining all possible type systems and each axes is the new abstraction. So going in some order from simple to more complex systems we arrive to what every programming language has :

**Values depending on values**
- This simply means that you have some sort of relation between variables if they are used in terms of each other. 

Example:

```
a :: Integer
b = even a :: Bool
-- here b depends on a 
```

After that we get to the second step which is

**Values depending on Types**

Example:

```
max :: Integer
max :: Double
```

We can look at this as if type is determining the values which function (or constant) `max` can hold.

Third step in our wonderfull type land is 

**Types depending on Types**

In order to demonstrate what we mean by this we will use Type Families. These [slides](https://cdepillabout.github.io/haskell-type-families-presentation/#/) do really good job in explaining what are type families.

Basically they provide a way to define a function that operates on types. 

The final step to dependent types is 

**Types depending on Values**

As I mentioned at the beginning Haskell still does not have native support for dependent types but we have a handfull of extensions that provide a way to touch on that. We are now arriving to the next step in understanding dependent types which is

## Local Assumptions



