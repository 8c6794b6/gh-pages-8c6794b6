---
title: Syntax for building rose tree
author: 8c6794b6
description: Showing couple syntaxes to write rose tree data
tags: datastructure, dsl, haskell, tree
---

**_Introduction_**

Suppose that, we want to write contents of following rose tree, by hand:

    0
    |
    +- 1
    |  |
    |  `- 2
    |
    +- 3
    |
    +- 4
    |  |
    |  +- 5
    |  |  |
    |  |  +- 6
    |  |  |
    |  |  `- 7
    |  |
    |  `- 8
    |
    `- 9

Using `Data.Tree.Tree` from `containers` package for our purpose, the definition
is:

    data Tree a = Node { rootLabel :: a
                       , subForest :: Forest a }

    type Forest a = [Tree a]

This document explores couple alternative ways to write rose tree data
structure, mainly focusing on situations when writing by hand.

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE FunctionalDependencies #-}
> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE UndecidableInstances #-}
> module Main where
>
> import Data.Tree
> import Control.Monad.Writer

It could be nice if we can write the rose tree in single line like:

> t3' :: Tree Int
> t3' = buildBranch $ pnode 0 (pnode 1 2) 3 (pnode 4 (pnode 5 6 7) 8) 9

Here, `pnode` is a polyvariadic function, treating the first argument as element
in itself, and rest as leaves. The code used for writing `t3'` is shown in Take
3.


**Take 1: _The Straightforward Way_**

In straightforward way, sample tree may written like below:

> t0 :: Tree Int
> t0 =
>     Node 0
>     [ Node 1
>       [ Node 2 [] ]
>     , Node 3 []
>     , Node 4
>       [ Node 5
>         [ Node 6 [], Node 7 []]
>       , Node 8 [] ]
>     , Node 9 [] ]

The sample tree containing 10 nodes already looks a bit cumbersome to write by
hand. From my point of view, things making the typing hard were:

1. Use of CAPITAL LETTERS: In general typing lower case letters are easier than
typing capital letters.
2. Use of brackets and commas: I realised that when typing numbers and commas,
my fingers are making quite a large movements on the keyboard. This coulbe be a
problem arise when typing numbers only. Say, if `Char` was used instead of
`Int`, may not be a problem.

**Take 2: _Slightly Less Straightforward Way_**

Pretty simple replacement of type constructors with functions with lower case
letters. Also using variant of `node` function named `leaf` which takes empty
list as second argument of `Node`. The sample tree looks like below:

> t1 :: Tree Int
> t1 =
>     node 0
>     [ node 1
>       [ leaf 2 ]
>     , leaf 3
>     , node 4
>       [ node 5
>         [leaf 6, leaf 7]
>       , leaf 8 ]
>     , leaf 9 ]
>
> node :: a -> [Tree a] -> Tree a
> node a fs = Node a fs
>
> leaf :: a -> Tree a
> leaf a = Node a []

Confirm that the two trees are identical:

    ghci> t1 == t0
    True

Now `t1` does not contain capital letters in its body, though nested lists might
still look clumsy.

**Take 3: _Reducing Commas And Brackets_**

Avoid typing commas and brackets (`,`, `[`, and `]`), let the `do notation` to
take care of node grouping. The sample tree looks like below:

> t2 :: Tree Int
> t2 = buildTree $ do
>     mnode 0 $ do
>         mnode 1 $ do
>             mleaf 2
>         mleaf 3
>         mnode 4 $ do
>             mnode 5 $ do
>                 mleaf 6 >> mleaf 7
>             mleaf 8
>         mleaf 9

Implementation is done with `Writer` monad with simple `DiffList`:

> newtype DiffList a = DiffList ([a] -> [a])
>     deriving (Monoid)
>
> instance Show a => Show (DiffList a) where
>     show (DiffList f) = show (f [])
>
> type TreeBuilder a = Writer (DiffList a) ()
>
> snoc :: a -> DiffList a
> snoc x = DiffList (x:)
>
> runTreeBuilder :: TreeBuilder (Tree a) -> [Tree a]
> runTreeBuilder builder = case runWriter builder of (_,DiffList f) -> f []
>
> buildTree :: TreeBuilder (Tree a) -> Tree a
> buildTree builder = case runTreeBuilder builder of
>     []  -> error "buildTree: empty tree"
>     t:_ -> t
>
> mnode :: a -> TreeBuilder (Tree a) -> TreeBuilder (Tree a)
> mnode x builder = tell (snoc (Node x (runTreeBuilder builder)))
>
> mleaf :: a -> TreeBuilder (Tree a)
> mleaf x = tell (snoc (Node x []))

Checking whether that `t2` is identical to `t0`:

    ghci> t2 == t0
    True


**Take 4: _Removing Do Notation_**

Use of `do notation` has freed us from using commans and brackets, though
introduced redundancy with `do`s. Introducing `Monoid` wrapper newtype and
polyvariadic function to remove `do`s.  Sample tree may looks like below:

> t3 :: Tree Int
> t3 = buildBranch $
>     pnode 0
>     (pnode 1
>      (pleaf 2))
>     (pleaf 3)
>     (pnode 4
>      (pnode 5
>       (pleaf 6) (pleaf 7))
>      (pleaf 8))
>     (pleaf 9)

Now `do`s are removed, thought increase of parenthesis is making the code quite
_lisp_y.  Implementation is heavily inspired by
[HSXML](http://okmij.org/ftp/Scheme/xml.html#typed-SXML).

> newtype Branch a = Branch (DiffList (Tree a))
>     deriving (Show, Monoid)
>
> class Monoid acc => BuildTree acc out ret | ret -> out where
>     build :: (acc->out) -> acc -> ret
>
> instance Monoid acc => BuildTree acc (Branch a) (Branch a) where
>     build f acc = f acc
>
> instance (BuildTree acc out ret, e ~ acc) => BuildTree acc out (e->ret) where
>     build f acc = \t -> build f (acc <> t)
>
> pnode :: BuildTree (Branch a) (Branch a) ret => a -> Branch a -> ret
> pnode x = build (\(Branch (DiffList b)) -> Branch (snoc (Node x (b []))))
>
> pleaf :: a -> Branch a
> pleaf x = Branch (snoc (Node x mempty))
>
> buildBranch :: Branch a -> Tree a
> buildBranch (Branch (DiffList ts)) = case ts [] of
>     []  -> error "buildBranch: empty branch"
>     t:_ -> t

Checking again:

    ghci> t3 == t0
    True


**_Some Thoughts_**

Other data type than `Data.Tree.Tree` could be used, though haven't explored.

In take 2, monadic approach may easy to combine with other monads with
`mtl`. For instance, use `State` monad and count the number of leaves while
traversing.

In take 3, by defining `Branch` as instance of `Num class`, `t3` could
rewritten in single line, which is shown as `t3'` at the beginning of this
document:

    t3' = buildBranch $ pnode 0 (pnode 1 2) 3 (pnode 4 (pnode 5 6 7) 8) 9

Checking:

    ghci> t3' == t0
    True

Purpose of making as `Num` instance is merely for helping syntax. Other functions
than `fromInteger` may left `undefined`:

> instance Num a => Num (Branch a) where
>     (+)         = undefined
>     (*)         = undefined
>     negate      = undefined
>     abs         = undefined
>     signum      = undefined
>     fromInteger = pleaf . fromInteger

GHC has restriction in context reduction stack, as of ghc-7.8.2, default size is
21. Polyvariadic function taking more than 22 arguments needs
`-fcontext-stack=N` option and increase the context stack with using large
context stack size _N_.
