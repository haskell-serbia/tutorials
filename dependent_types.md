# Dependent Types in Haskell

Dependent types enable us to write more safe programs and move some of the possible runtime bugs to compile time errors. They enable us to "lift" terms to the type level. 

Haskell currently does not support dependent types natively but there are certain extensions that help us to get there.

It is not as elegant as it would be if we had built in support for this but [Richard Eisenberg](https://typesandkinds.wordpress.com/) is busy doing a great work to make this happen. 

We will go trough different steps in explaining dependent types in Haskell:

* Terms vs. Types
* Type level data
* Lambda Cube
* Local Assumptions
* Generic 
* Sigma and Pi

## Terms vs. Types
Difference between the terms and types can be looked at in few different ways. 

First one would be that if you look at type annotation:
```
term :: type :: kind
```
You can ignore kinds for now, they are to types what types are to terms - so like type of the type constructor. This sounds complicated but I promise it is not, just think of them as _types one level up_. You can read about kinds [here](https://wiki.haskell.org/Kind)

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

In contrast to this types can also have a _type level data_. That is the data that lives on the type level and does not have associated set of inhabitants. 

Haskell is famous for its _If it compiles - it works_ approach, which is very true but we are haskellers - we always want more type safety and abstraction right ? 

We can use type level data to provide extra safety to our programs since that allows us to _describe_ how our types should be constructed thus eliminating more runtime errors. 

Here is a small example from `GHC` [manual](https://ghc.readthedocs.io) that provides a way of defining sort of an accessor with convenient type level _String_ (`Symbol`) 
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

[Lambda Cube](https://en.wikipedia.org/wiki/Lambda_cube) is the term from Type Theory and Math Logic. It builds on top of [lambda calculus](https://en.wikipedia.org/wiki/Lambda_calculus) defining all possible type systems where each axes is the new abstraction. So going in some order from simple to more complex systems we first arrive to what every programming language has :

**Values depending on values** (_functions_)
- This simply means that you have some sort of relation between variables if they are used in terms of each other. 

Example:

```
a :: Integer
b = even a :: Bool
-- here b depends on a 
```

After that we get to the second step which is

**Values depending on Types** (_classes_)

Example:

```
max :: Integer
max :: Double
```

We can look at this as if type is determining the values which constant `max` can hold. This is a basic relation between values and types, values need to be of some type in Haskell.

Third step in our wonderfull type land is 

**Types depending on Types** (_type functions_)

In order to demonstrate what we mean by this we will use Type Families. These [slides](https://cdepillabout.github.io/haskell-type-families-presentation/#/) do really good job in explaining what are type families.

Basically they provide a way to define a function that operates on types. Depending on the concrete implementation we can return different types for different instances of the typeclass or at least we have that choice.

This leads us to the final step to dependent types which is: 

**Types depending on Values** (_dependent types_)

As I mentioned at the beginning Haskell still does not have native support for dependent types but we have a handfull of extensions that provide a way to touch on that. We are now arriving to the next step in understanding dependent types which is

## Local Assumptions

GADTs or Generalized Algrbraic DataTypes is the Haskell extension that provides us with _local assumptions_. What does that mean? 

Well, let us look at some of the standard Haskell datatypes :

```
data Maybe a = Just a | Nothing
data Either a b = Left a | Right b
```
If we rewrote these types with hypotetical syntax we would come to this:
```
data Maybe a = Just a
     Maybe a = Nothing

data Either a b = Left a
     Either a b = Right b
```
and this can inform us that we are really limited with what we do on left side of the `=` sign. 

Problem with this is that we don't get to do pattern matching on the type constructors which in turn means that we can only have free type variables on the left side.

This is where *GADTs* language extension comes to play. It allows us to define type contraints on the type variable which limits the possible type variables that can be used to construct a type and later in the code have *local assumptions* when pattern matching on the data constructor which was used to constuct a type. 

Lets take a look at some example code:

```
data IntOrString a where
    IntConstructor    :: a ~ Int    => IntOrString a -- a has a type constraint to Int
    StringConstructor :: a ~ String => IntOrString a  -- a has a type constraint to String
```
This is desugared version of the more convenient syntax that we usually use

```
data IntOrString a where
    IntConstructor    :: Int    -> IntOrString a
    StringConstructor :: String -> IntOrString a
```

The classic `Maybe` datatype would look like this if written with GADTs

```
data Maybe a where
    Just    :: a -> Maybe a
    Nothing :: Maybe a
```
And here is example of the pattern match:

```
wasItStringOrInt :: IntOrString a -> String
wasItStringOrInt x =
    case x of
        -- Local assumptions - Local because it is limited to only one case branch 
        -- and assumption because we assume what type is our type parameter
        IntConstructor a    -> "Constructed with Int"      -- a ~ Int
        StringConstructor b -> "Constructed with String    -- a ~ String
        
-- ghci
λ> let x = IntConstructor 42
λ> wasItStringOrInt x
"Constructed with Int"
```





