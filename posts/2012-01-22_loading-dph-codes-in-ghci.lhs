---
title: Loading dph codes in ghci
author: 8c6794b6
date: January 22, 2012
tags: haskell, dph, ghci
description: How to load dph codes within ghci
---

In haddock of dph-par package, we see functions like below:

> zipP :: [:a:] -> [:b:] -> [:(a, b):]
> {-# NOINLINE zipP #-}
> zipP !_ !_ = [::]
> {-# VECTORISE zipP = zipPA #-}

Above difinition is from
[dph-par-0.5.1.1](http://hackage.haskell.org/packages/archive/dph-par/0.5.1.1/doc/html/src/Data-Array-Parallel.html#zipP).
From what its written, `zipP` looks like a function that returning an
empty array. Though after compiling the code with appropriate options,
`zipP` indeed work as parallel variant of `zip` for standard list.

As of GHC-7.2.2 and dph-par-0.5.1.1, we cannot directly load codes
with parallel array in ghci. One way to load them in ghci is, prepare
a wrapper with fixing the type of array element, and compile to object
code.

Suppose that we have a module containing parallel array functions:

> {-# LANGUAGE ParallelArrays #-}
> {-# OPTIONS_GHC -fvectorise #-}
> -- VectroseMe.hs
> module VectoriseMe where
>
> import Data.Array.Parallel
> import Data.Array.Parallel.PArray
>
> myZipP :: [:a:] -> [:b:] -> [:(a, b):]
> myZipP as bs = zipP as bs
>
> myZipPA :: PArray a -> PArray b -> PArray (a, b)
> myZipPA as bs = toPArrayP (myZipP (fromPArrayP as) (fromPArrayP bs))
> {-# NOINLINE myZipPA #-}
>
> myZipIntP :: [:Int:] -> [:Int:] -> [:(Int, Int):]
> myZipIntP as bs = zipP as bs
>
> myZipIntPA :: PArray Int -> PArray Int -> PArray (Int, Int)
> myZipIntPA as bs = toPArrayP (myZipIntP (fromPArrayP as) (fromPArrayP bs))
> {-# NOINLINE myZipIntPA #-}

And a wrapper which imports above module:

> -- Wrapper.hs
> module Wrapper where
>
> import Data.Array.Parallel
> import Data.Array.Parallel.PArray (fromList)
>
> import VectoriseMe
>
> main :: IO ()
> main = do
>   putStrLn $ concat ["myZip_test:", "\n  ", show myZip_test]
>   putStrLn $ concat ["myZipInt_test:", "\n  ", show myZipInt_test]
>
> pa1, pa2 :: PArray Int
> pa1 = fromList [1,2,3]
> pa2 = fromList [4,5,6]
>
> myZip_test :: PArray (Int, Int)
> myZip_test = myZipPA pa1 pa2
>
> myZipInt_test :: PArray (Int, Int)
> myZipInt_test = myZipIntPA pa1 pa2

Loading Wrapper.hs in ghci and invoking the main function:

    | $ ghci -fdph-par -fobject-code -fforce-recomp Wrapper.hs
    | GHCi, version 7.2.2: http://www.haskell.org/ghc/  :? for help
    | Loading package ghc-prim ... linking ... done.
    | Loading package integer-gmp ... linking ... done.
    | Loading package base ... linking ... done.
    | Loading package array-0.3.0.3 ... linking ... done.
    | Loading package old-locale-1.0.0.3 ... linking ... done.
    | Loading package time-1.2.0.5 ... linking ... done.
    | Loading package random-1.0.1.1 ... linking ... done.
    | Loading package primitive-0.3.1 ... linking ... done.
    | Loading package vector-0.7.1 ... linking ... done.
    | Loading package dph-base-0.5.1.1 ... linking ... done.
    | Loading package dph-prim-interface-0.5.1.1 ... linking ... done.
    | Loading package dph-prim-seq-0.5.1.1 ... linking ... done.
    | Loading package old-time-1.0.0.7 ... linking ... done.
    | Loading package dph-prim-par-0.5.1.1 ... linking ... done.
    | Loading package containers-0.4.1.0 ... linking ... done.
    | Loading package filepath-1.2.0.1 ... linking ... done.
    | Loading package unix-2.5.0.0 ... linking ... done.
    | Loading package directory-1.1.0.1 ... linking ... done.
    | Loading package pretty-1.1.0.0 ... linking ... done.
    | Loading package process-1.1.0.0 ... linking ... done.
    | Loading package Cabal-1.12.0 ... linking ... done.
    | Loading package bytestring-0.9.2.0 ... linking ... done.
    | Loading package binary-0.5.0.2 ... linking ... done.
    | Loading package bin-package-db-0.0.0.0 ... linking ... done.
    | Loading package hoopl-3.8.7.1 ... linking ... done.
    | Loading package hpc-0.5.1.0 ... linking ... done.
    | Loading package template-haskell ... linking ... done.
    | Loading package ghc-7.2.2 ... linking ... done.
    | Loading package dph-par ... linking ... done.
    | Loading package ffi-1.0 ... linking ... done.
    | [1 of 2] Compiling VectoriseMe      ( VectoriseMe.hs, VectoriseMe.o )
    | [2 of 2] Compiling Wrapper          ( Wrapper.hs, Wrapper.o )
    | Ok, modules loaded: Wrapper, VectoriseMe.
    | ghci> main
    | myZip_test:
    |   fromList<PArray> []
    | myZipInt_test:
    |   fromList<PArray> [(1,4),(2,5),(3,6)]

We see that `myZip_test` is showing empty list, and `myZipInt_test` is
showing the zipped contents.