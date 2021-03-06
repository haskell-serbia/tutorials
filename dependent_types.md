# Dependent Types in Haskell

*This topic is considered as advanced and some prior knowledge of Haskell is assumed although I will try to provide links for the terms not explained in the article*.

_Why_ ?

Dependent types help to form a proof that the most critical features work the way we want them and all that in compile time. We can form specific set of types that will ensure all invariants program can have will work properly. 

Dependent types use type level functions that are reduced using term level data.

Languages like Idris, Agda or Coq support dependent types natively and Haskell has certain extensions that help us simulate them.

We will go trough different steps in explaining dependent types in Haskell:

* Terms vs. Types
* Type Level Data
* Lambda Cube
* Local Assumptions
* Generic Programming
* Sigma and Pi

## Terms vs. Types
Difference between the terms and types can be looked at in a few different ways. 

First one would be that if you look at a type/kind annotation:
```
term :: type :: kind
```
_term_ is the thing to the left of the type annotation (`::`) and _type_ is to the right of it.
You can ignore kinds for now, they are to types what types are to terms - so like type of a type constructor. You can think of them as _types one level up_. Read about kinds [here](https://wiki.haskell.org/Kind)

We can also say something that applies to standard Haskell (without extensions) and that is that terms are present at runtime while types get erased at runtime. This statement is not true when it comes to dependent types but it serves well in the path of our understanding of the topic.

## Type level data
We can say for a type that it has a set of possible values that correspond to it. So `Void` is a type with zero inhabitants - empty set , `Unit` has a single element set (`()`) , `Bool` has two element set (`True` and `False`) and so on. Here is a small example of this

```
data Void 
data Unit = U -- unit is built-in type and its actual definition is data () = () 
data Bool = True | False
```

In contrast to this types can also contain _type level data_. That is the data that lives on the type level and does not have associated set of inhabitants. 

Here is a small example that provides a way of defining a type level _String_ (`Symbol`) 

```
type Greetings = "Hello" :: Symbol
data Label Greetings = Get
```

Here type `Label` accepts a type parameter which must be of type `Symbol`. Type level data is something that lives on the level of types but is not a type.

## Lambda Cube

[Lambda Cube](https://en.wikipedia.org/wiki/Lambda_cube) is the framework for exploring the forms of abstraction. Starting in some order from simple lambda calculus to calculus of constructions (higher-order dependently typed polymorphic lambda calculus) we first arrive to what every programming language has :

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
maxBound :: Int
maxBound :: Double
```

We can see here that the type is determining the values `maxBound` will have. Values depend on the type they are instantiated with so the type to the right of the `::` determines the values to the left of the `::`.


**Types depending on Types** (_type functions_)

In order to demonstrate what we mean by this we will use Haskell extension called Type Families. 

Type Families provide pattern matching on a type parameter.
Here is an example from Haskell [wiki](https://wiki.haskell.org/Haskell)

```
-- Declare a list-like data family
data family XList a
 
-- Declare a list-like instance for Char
data instance XList Char = XCons !Char !(XList Char) | XNil
 
-- Declare a number-like instance for ()
data instance XList () = XListUnit !Int


```

Here we are providing two instances of the same data type. One can be constructed when using `Char` for the type constructor parameter and the other one when using  `()`.

Following example uses type level funcion on a type level data to reverse a list

```
type family Reverse (xs :: [a]) :: [a] 
```

Using Type Families we can say that type used as a parameter to a type constructor determines the type we will be able to construct - so type depending on another type. 

Haskell is famous for its _If it compiles - it works_ approach, which is very true but we are haskellers - we always want more type safety and abstraction. 

This leads us to the final step to dependent types which is: 

**Types depending on Values** (_dependent types_)

As I mentioned at the beginning Haskell still does not have native support for dependent types but we have a handful of extensions that provide a way to touch on that. We are now arriving to the next step in understanding dependent types which is

## Local Assumptions

GADTs or Generalized Algebraic DataTypes is the Haskell extension that provides us with _local assumptions_. What does that mean? 

GADTs provide us with a way to have richer return types than vanilla ADTs. If we look at the GHC manual example

```
data Term a where
    Lit    :: Int -> Term Int
    Succ   :: Term Int -> Term Int
    IsZero :: Term Int -> Term Bool
    If     :: Term Bool -> Term a -> Term a -> Term a
    Pair   :: Term a -> Term b -> Term (a,b)
```
We notice that the return type is not always `Term a` as it would be with normal ADTs. When pattern matching on type constructors that are constructed using GADTs syntax we are able to have type refinement to specific constructor so `a` can be refined to `Int` in following function

```
eval :: Term a -> a
eval Lit a = ... 
```
Since pattern matching reduced to this branch and return type is `Term Int` 

```
Lit    :: Int -> Term Int
```

When pattern matching on type constructor using GADTs we get assumption that the type was constructed with certain type parameter/s. This is what we also get with data families but difference is that we get _local_ assumptions since we are able to use pattern matching on polymorphic type variables , so all possible results defined in GADt since they have fixed set of constructors.

Lets take a look at some example code:

```
data IntOrString a where
    IntConstructor    :: a ~ Int    => IntOrString a -- a has a type constraint to Int
    StringConstructor :: a ~ String => IntOrString a -- a has a type constraint to String
```
This is desugared version of the more convenient syntax that we usually use

```
data IntOrString a where
    IntConstructor    :: Int    -> IntOrString Int
    StringConstructor :: String -> IntOrString String
```

Here is example of the pattern match:

```
wasItStringOrInt :: IntOrString a -> IntOrString b -> [Char]
wasItStringOrInt x y =
-- Local assumptions - Local because it is limited to only one case branch
-- and assumption because we can assume of what type is our type parameter
  case x of
    IntConstructor x' -> case y of
      IntConstructor   y'  -> show $ x' + y'    -- x ~ Int y ~ Int
      StringConstructor y' -> (show x') ++  y'  -- x ~ Int y ~ String
    StringConstructor x' -> case y of
      IntConstructor    y' -> x' ++ (show y')   -- x ~ String y ~ Int
      StringConstructor y' -> x' ++ y'          -- x ~ String y ~ String

        
-- ghci
λ> let xx = IntConstructor  (42 :: Int)
λ> let yy = StringConstructor "Haskell rocks!"
λ> wasItStringOrInt  yy xx
"Haskell rocks!42"
```

## Generic programming

Let us now build our intuition about types in general and also dependent types.

If we look at a normal Haskell Algebraic Data Type `T`

```
data T = A Int Int
  | B String String String
  | C () Void
  | D
```
<sub>Haskell is among small number of languages equipped with sum types. Read about them [here](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/sum-types) .</sub>

We can say that `T` is a sum of products `A B C D`. In order to understand sum we can define this and every other ADT only using
`Either`, `() (unit)` , `(,) tuple`  and `((->) r )` function type. So our ADT could be encoded like this using infix notation

```
type T' = (Int, Int) `Either` (String, String, String) `Either` ((), Void) `Either` ()
```

Tuples provide us with products, Either provides us with sums, unit is empty constructor and function type can take type/value and yield new type/value. 

If we understand these simple types than we understand Algebraic Data Types.

If we understand Σ (sigma) and Π (pi) we understand dependent types. 

<sub>You can read about the theory behind all of this [here](https://en.wikipedia.org/wiki/Intuitionistic_type_theory) .</sub>

## Σ and Π

First we will look at some pseudo code so we can explain easier what is going on and after that we will take a look at the example that actually compiles.

Sigma type is like a tuple but with some caveats

```
  (A     , B) 
Σ (x :: A) B(x)
    
```
Here `x` is of type `A` and we are able to use it inside some type level function `B(x)`.

One possible sigma type could be

```
Σ (x :: Bool) (if x then Int else String) -- this if statement can be implemented as a type family in haskell

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
What can we do with it?
We can pattern match on it!

```
f :: (x :: Bool) (if x then Int else String) -> String
f (x,y) = case x of
  True -> show y -- if x is True then y :: Int
  False -> y     -- if x is False then y :: String
```
As soon as we pattern match we are able to use local assumptions about `y` which is the term level data and we know that `y` can be either of type `Int` or of type `String`. Any other type would have caused compile time error and in this way we are able to prevent our program from working with types we don't want and prove that our program is correct at compile time!
How cool is that!


**∑** is type level generalization of a sum types - it is a sum of all possible first components of a tuple (True + False in our case).

**Π** is type level generalization of `Product` types.

Let's look at the definition

```
    A   ->  B
Π  (x :: A) B(x)
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

Let us now look at the example of some fictive web application.
We will use dependent types to prevent wrong behavior at compile time. Let's imagine we have the option to create either `GET` or `POST` 
request. `GET` request can contain only unit `()` and `POST` request only `Maybe Body`. We will encode this in such way that we will not be able to compile program that does not do what is described.

```
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Kind

type Body = [Char]

data Method
  = GET
  | POST
  deriving (Show)

data SMethod m where
  SGET  :: m ~ 'GET  => SMethod m
  SPOST :: m ~ 'POST => SMethod m

deriving instance Show (SMethod m)

type family IfGetThenUnitElseMaybeBody (m :: Method) :: Type where
  IfGetThenUnitElseMaybeBody 'GET = ()
  IfGetThenUnitElseMaybeBody 'POST = Maybe Body
  
-- this type should remind you of our ∑ type 
-- Σ (x :: Bool) (if x then Int else String) 
data Request m =
  Req (SMethod m)
      (IfGetThenUnitElseMaybeBody m)


mkSMethod :: Method -> Either (SMethod 'GET) (SMethod 'POST)
mkSMethod m =
    case m of
        GET  -> Left SGET
        POST -> Right SPOST
        
mkValidRequest :: Method -> Either (Request 'GET) (Request 'POST)
mkValidRequest m = do
  let requestBody = (Just "POST BODY" :: Maybe Body)
  let sm = mkSMethod m
  case sm of
    Left  SGET  -> Left $ Req SGET ()
    Right SPOST -> Right $ Req SPOST requestBody


main :: IO ()
main = return ()

```
We can compile this

```
[1 of 1] Compiling Main      
Linking .stack-work/dist/x86_64-osx/Cabal-1.24.2.0/build/ ...
xxx-0.1.0.0: copy/register
Installing executable(s) 
```
And test it in ghci session

```
λ> :m Data.Either Main 
λ> let x = mkValidRequest GET
λ> isLeft x
True
λ> isRight x
False
λ> 
```
When reduced `a` would be of type `Left Request 'GET`.

Now let's change something so that we return string "get" in case of `GET` request instead of `()`

```
-- let x = Req SGET ()
let x = Req SGET "get"
```

If we try to compile this we will get a type error

```
[1 of 1] Compiling Main 
Main.hs:48:26: error:
    • Couldn't match type ‘()’ with ‘[Char]’
      Expected type: IfGetThenUnitElseMaybeBody 'GET
        Actual type: [Char]
    • In the second argument of ‘Req’, namely ‘"get"’
      In the expression: Req SGET "get"
      In an equation for ‘x’: x = Req SGET "get"

    Process exited with code: ExitFailure 1
```

The same thing would happen in case we try to return anything else except `()` for `GET` and `Maybe Body` for `POST` request.
Now we see from this simple example why dependent types are considered excellent tool when program correctness is of ultimate importance.
There is no funky runtime behavior, our app will blow up in our face at compile time if we don't write the only correct version of the program.

I agree this looks cumbersome and involves a lot of boilerplate (this is from perspective of a Haskeller, Java programmers will not notice anything :) ) but currently this is the only way to mimic dependent types until they are a part of the language.


Concepts described here are hard and in case you could not follow all don't beat your self up. Haskell can be hard as it is without language extensions. Read online resources and try again until it "sinks in". Most of us don't write programs for NASA everyday and Haskell Type System is already powerful enough even without dependent types. That said it doesn't mean you should not explore and play with different concepts that will probably be really important in the future.

