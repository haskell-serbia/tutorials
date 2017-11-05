# Dependent Types in Haskell


_Why_ ?

Dependent types help to form a proof that the most critical properties in our program work the way we want them and all that in compile time. We form very specific types that ensure all invariants program can have to work properly. They enable us to _lift_ terms to the type level and have types be dependent on them. 

Dependent types use type level functions that are reduced using term level data.

Languages like Idris Agda or Coq support dependent types natively and Haskell has certain extensions that help us to simulate them.

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

GADTs or Generalized Algebraic DataTypes is the Haskell extension that provides us with _local assumptions_. What does that mean? 

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
    StringConstructor :: a ~ String => IntOrString a -- a has a type constraint to String
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

## Generic

Lets now build our intuition about types in general and also dependent types.

If we look at a normal Haskell Algebraic Data Type `T`

```
data T = A Int Int
  | B String String String
  | C () Void
  | D
```

We can say that `T` is a sum of products `A B C D`. In order to understand sum we can define this and every other ADT only using
`Either`, `() (unit)` , `(,) tuple`  and function type. So our ADT could be encoded like this using infix notation

```
type T' = (Int, Int) `Either` (String, String, String) `Either` ((), Void) `Either` ()
```

Tuples provide us with products, Either provides us with sums, unit is empty constructor and function type is just a built in function. 

If we understand these simple types than we understand Algebraic Data Types.

If we understand Σ (sigma) and Π (pi) we understand dependent types. 

You can see the theory behing all of this [here](https://en.wikipedia.org/wiki/Intuitionistic_type_theory) .

## Σ and Π

First we will look at some pseudo code so we can explain easier what is going on and after that we will take a look at the example that actually compiles.

Sigma type is like a tuple but with some caviats

```
     (A     , B) 
Σ :: (x :: A) B(x)
    
```
Here `x` is of type `A` and we are able to use it inside some type level function `B(x)`.

One possible sigma type could be

```
Σ :: (x :: Bool) (if x then Int else String) -- this if statement can be implemented as a type family in haskell

-- possible values
(True, 42)
(False, "abc")
``` 
And if we try to use this in a function we can have something like:

```
f :: ∑ (x :: Bool) (if x then Int else String) -> String
f (x,y) = ???
```
So now we have a function from `sigma` to `String` , we know that `x` is of type `Bool` but we have no idea what `y` is .
What can we do with it ?
We can pattern match on it!

```
f :: (x :: Bool) (if x then Int else String) -> String
f (x,y) = case x of
  True -> show y -- if x is True then y ~ Int
  False -> y     -- if x is False then y ~ String
```
As soon as we pattern match we are able to use local assumptions about `y` which is a term level data and we know that `y` can be either `Int` or `String`. Any other type would have caused compile time error and in this way we are able to prevent our programs from working with some random types that we don't want and prove that our program is correct at compile time!
How cool is that!


**∑** is type level generalization of a `Sum` types - it is a sum of all possible first components of a tuple (True + False in our case).

**Π** is type level generalization of `Product` types.

Let's look at the definition

```
      A   ->  B
Σ :: (x :: A) B(x)
```
The type of the result of the `B` will depend on its input.
```
f :: Π (x :: Bool) (if x then Int else String) -> String
f x = case x of
  True -> 42
  False -> "abc"  
```
So what we get back from a Π type is a function. Why we say it is a product type ? You can think of it like this - you are able to get back values from a Π type which is not a case for the ∑ type. ∑ type can have as a return type one of the case branches while from Π you can extract values much like from a product type in Haskell

 ```
 f True = 42
 f False = "abc"
 ```

Ok enough with the theory you say, let's look at a simple example using real code
