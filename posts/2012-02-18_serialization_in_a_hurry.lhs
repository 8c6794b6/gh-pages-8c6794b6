---
title: Serialization in a hurry
author: 8c6794b6
date: Feburary 18, 2012
tags: haskell, generic, ghc
description: Serializing user defined data in a hurry
---

I need to serialize below data in 5 minutes:

    | data Foo = Foo
    |  { one   :: Int
    |  , two   :: (Maybe Double, String)
    |  , three :: Vector Double
    |  }

Requires ghc-7.2.2 or above, and
[cereal](http://hackage.haskell.org/package/cereal) package.

> {-# LANGUAGE DeriveGeneric #-}
> module SerializationInAHurry where

We are using `DeriveGeneric` language pragma.

> import GHC.Generics
> import Data.Serialize
> import Data.Vector (Vector(..))
> import qualified Data.ByteString as B
> import qualified Data.Vector as V

The target data type to serialize.

> data Foo = Foo
>  { one   :: Int
>  , two   :: (Maybe Double, String)
>  , three :: Vector Double
>  } deriving (Generic, Show, Eq)

And sample data with target data type.

> foo1 :: Foo
> foo1 = Foo
>   { one   = 100
>   , two   = (Just 8.5, "eight point five")
>   , three = V.fromList [11,13..21]
>   }

As haddock document in cereal package notes, when the target data type
has `Generic` instance, `Serialize` class will use it. What we need is
to make instance of `Serialize` with empty body.

`Data.Generics.Generic` class has couple instances already defined.

    | instance Generic [a] -- Defined in `GHC.Generics'
    | instance Generic Ordering -- Defined in `GHC.Generics'
    | instance Generic Int -- Defined in `GHC.Generics'
    | instance Generic Float -- Defined in `GHC.Generics'
    | instance Generic Double -- Defined in `GHC.Generics'
    | instance Generic Char -- Defined in `GHC.Generics'
    | instance Generic Bool -- Defined in `GHC.Generics'
    | instance Generic (Maybe a) -- Defined in `Data.Maybe'
    | instance Generic (Either a b) -- Defined in `Data.Either'
    | instance Generic (a, b, c, d, e, f, g) -- Defined in `GHC.Tuple'
    | instance Generic (a, b, c, d, e, f) -- Defined in `GHC.Tuple'
    | instance Generic (a, b, c, d, e) -- Defined in `GHC.Tuple'
    | instance Generic (a, b, c, d) -- Defined in `GHC.Tuple'
    | instance Generic (a, b, c) -- Defined in `GHC.Tuple'
    | instance Generic (a, b) -- Defined in `GHC.Tuple'
    | instance Generic () -- Defined in `GHC.Tuple'

The only missing definition we need is:

> instance Serialize a => Serialize (Vector a) where
>   put = put `fmap` V.toList
>   get = V.fromList `fmap` get

There should be other way which shows better performance for
serializing `Vector`, but recall the title of this post. Now we are
ready to serialize.

> instance Serialize Foo

Testing it with write to file, reading from the file and comparing
with original:

> main :: IO ()
> main = do
>   let file = "foo1.data"
>   B.writeFile file (encode foo1)
>   res <- decode `fmap` B.readFile file :: IO (Either String Foo)
>   case res of
>      Right foo1' -> print $ foo1 == foo1'
>      Left  err   -> error err

Result:

    | ghci> main
    | True
