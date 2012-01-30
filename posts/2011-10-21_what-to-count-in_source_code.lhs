---
title: What to count in source code
author: 8c6794b6
date: Octover 21, 2011
tags: haskell, arc
description: Counting expressions in haskell source code
---
Below is a sample arc code:

     | (defop said req
     |   (aform [onlink "click here" (pr "you said: " (arg _ "foo"))]
     |     (input "foo")
     |     (submit)))

In an article
[arc challenge](http://paulgraham.com/arcchallenge.html), Paul Graham was
counting size of codetree of arc program. He counted above arc program
as 21 nodes: 14 leaves + 7 interior.

How can we count the size of haskell codetree?

> module Main where
>
> import Control.Monad
> import Data.Generics
> import Data.Tree
> import System.Environment (getArgs)

We could use other module than Language.Haskell.Syntax.
e.g. Language.Haskell.Exts from haskell-src-exts.

> import Language.Haskell.Parser
> import Language.Haskell.Syntax

This code could be run from command line. Parses the contents and
count the number of codesize in all given file paths.

> main :: IO ()
> main =  getArgs >>= mapM_ (readFile >=> count namesAndLits)

Below is arc expression written with Tree data type. Last node "submit" has a
child element with empty string, since it was parenthesized explicitly.

> arc :: Tree String
> arc =
>   Node "defop"
>     [Node "said" []
>     ,Node "req" []
>     ,Node "aform"
>       [Node "onlink"
>         [Node "click here" []
>         ,Node "pr"
>           [Node "you said: " []
>           ,Node "arg"
>             [Node "_" []
>             ,Node "foo" []]]]
>         ,Node "input"
>           [Node "foo" []]
>         ,Node "submit"
>           [Node "" []]]]

Viewing this expression:

    | *Main> putStrLn $ drawTree arc
    | defop
    | |
    | +- said
    | |
    | +- req
    | |
    | `- aform
    |    |
    |    +- onlink
    |    |  |
    |    |  +- click here
    |    |  |
    |    |  `- pr
    |    |     |
    |    |     +- you said:
    |    |     |
    |    |     `- arg
    |    |        |
    |    |        +- _
    |    |        |
    |    |        `- foo
    |    |
    |    +- input
    |    |  |
    |    |  `- foo
    |    |
    |    `- submit
    |       |
    |       `-

To count number of leaves:

> numLeaf :: Tree a -> Int
> numLeaf = length . flatten

And number of interior nodes:

> numInterior :: Tree a -> Int
> numInterior t = case t of
>   Node _ [] -> 0
>   Node _ xs -> 1 + sum (map numInterior xs)

And getting the sum:

> countArc :: Tree a -> Int
> countArc t = numLeaf t + numInterior t

Number of expression is:

    | *Main> countArc arc
    | 22

It's 22, not 21, due to the addition of extra empty node in the end.

So, that was how we can count expression of lisp. Then how to count like for
haskell expression?  One way is to count occurance of specific data types
found in Language.Haskell.Syntax module. When we found HsName datatype in
source code, increase the number of codesize by 1. Below is the definition of
HsName.

    | data HsName = HsIdent String | HsSymbol String

Since most data types in Language.Haskell.Syntax are defined as instance of Data
and Typeable, we can use generic functions.

> anyName :: HsName -> Int
> anyName _ = 1

Traversing over module:

> names :: HsModule -> Int
> names = everything (+) (0 `mkQ` anyName)

Worker for parsing contents of source code, and applying the counter function.

> count :: (HsModule -> Int) -> String -> IO ()
> count f code = do
>   case parseModule code of
>     ParseOk mdl@(HsModule _ name _ _ _) ->
>       putStrLn $ unwords [show $ f mdl, "\t", show name]
>     ParseFailed loc err -> print loc >> putStrLn err

Try it:

> sample1 :: String
> sample1 = "x = 100"

    | *Main> count names sample1
    | 2        Module "Main"

Seems like working. How about another expression?

> sample2 :: String
> sample2 = "x = 100 + 2"

Outputs:

    | *Main> count names sample2
    | 3        Module "Main"

Why above codesize counted as 3? Not 4, code size of sample1 + 2?

We should have missed some data type to count. For second try, counting
HsLiteral and HsName.

    | data HsLiteral
    |   = HsChar Char
    |   | HsString String
    |   | HsInt Integer
    |   | HsFrac Rational
    |   | HsCharPrim Char
    |   | HsStringPrim String
    |   | HsIntPrim Integer
    |   | HsFloatPrim Rational
    |   | HsDoublePrim Rational

> anyLiteral :: HsLiteral -> Int
> anyLiteral _ = 1

> namesAndLits :: HsModule -> Int
> namesAndLits = everything (+) (const 0 `extQ` anyName `extQ` anyLiteral)

Trying again:

    | *Main> count namesAndLits sample1
    | 3        Module "Main"
    | *Main> count namesAndLits sample2
    | 5        Module "Main"

Getting better, but this is counting leaves only.

Since all haskell expression in general is a function that takes
single argument, and giving back another function, every expression other
than constant value has child element. I'm not sure what we should count as
interior node in expression.
