---
title: Cause of 3 nano seconds difference
author: 8c6794b6
date: April 22, 2012
tags: haskell, performance
description: Seeking for faster way than looping with unboxed vector.
---

`Unboxed Vector`, in
[vector](http://hackage.haskell.org/package/vector) package is
efficient data structure. Iterating over unboxed value is pretty fast.

Is there any faster way to iterate than unboxed vector?

Interest of comparision here is *efficiency of iteration*. Not the
body contents of work in each iteration, but overhead we get from the
loop.

> {-# LANGUAGE BangPatterns #-}
> module Main where
>
> import Criterion.Main (bench, defaultMain, nf)
> import qualified Data.Vector.Unboxed as U

Couple type synonyms, and body function to apply inside the loop.

> type Updater a = a -> a
> type Work a = Updater a -> a -> Int -> a
>
> add1 :: Updater Int
> add1 = (+1)

Using unboxed vector to loop for given time. Using `foldl'` and
`replicate`.


> work01 :: Work Int
> work01 f n m = U.foldl' (\x _ -> f x) n (U.replicate m ())

Invoking `work01` with `add1`. Below will apply `add1` to 0 for 100
times.

    ghci> work01 add1 0 100
    100

Manual recursive loop function with wrapper strict data type.

> data P = P {-# UNPACK #-} !Int {-# UNPACK #-} !Int
>
> work02 :: Work Int
> work02 f n0 m = go (P 0 n0) where
>   {-# INLINE go #-}
>   go !(P i n) | i == m    = n
>               | otherwise = go (P (i+1) (f n))

Again, applying `add1` to 0 for 100 times.

    ghci> work02 add1 0 100
    100

Main for taking benchmark looks like below.

> main_one :: IO ()
> main_one = do
>   let k = 2 ^ 10
>   defaultMain
>     [ bench "unboxed vector" (nf (work01 add1 0) k)
>     , bench "hand written recursion" (nf (work02 add1 0) k)
>     ]

Compiling with couple optimization options:

    $ ghc -O3 -fllvm -optl-O3 -main-is main_one -o a.out loop.lhs

The benchmark result:

    benchmarking unboxed vector
    mean: 52.56010 ns, lb 52.49446 ns, ub 52.64269 ns, ci 0.950
    std dev: 376.4229 ps, lb 310.1393 ps, ub 458.0804 ps, ci 0.950

    benchmarking hand written recursion
    mean: 55.53802 ns, lb 55.51631 ns, ub 55.56074 ns, ci 0.950
    std dev: 113.1906 ps, lb 103.0162 ps, ub 125.4034 ps, ci 0.950

In this micro benchmark, looping with unboxed vector was about 3 nano
seconds faster than manual strict recursion.

What is the cause of this 3 nano seconds difference? Seems like it's a
goot time to have a look at GHC core. Dumped core of `work01` looks
like below:

    Main.work01 :: Main.Work GHC.Types.Int
    [GblId,
     Arity=3,
     Caf=NoCafRefs,
     Str=DmdType LS(A)U(L)m,
     Unf=Unf{Src=InlineStable, TopLvl=True, Arity=0, Value=True,
             ConLike=True, Cheap=True, Expandable=True,
             Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=True)
             Tmpl= Main.work1
                   `cast` (<Main.Updater GHC.Types.Int>
                           -> <GHC.Types.Int>
                           -> <GHC.Types.Int>
                           -> Data.Vector.Fusion.Util.NTCo:Id <GHC.Types.Int>
                           :: (Main.Updater GHC.Types.Int
                               -> GHC.Types.Int
                               -> GHC.Types.Int
                               -> Data.Vector.Fusion.Util.Id GHC.Types.Int)
                                ~#
                              (Main.Updater GHC.Types.Int
                               -> GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Int))}]
    Main.work01 =
      Main.work1
      `cast` (<Main.Updater GHC.Types.Int>
              -> <GHC.Types.Int>
              -> <GHC.Types.Int>
              -> Data.Vector.Fusion.Util.NTCo:Id <GHC.Types.Int>
              :: (Main.Updater GHC.Types.Int
                  -> GHC.Types.Int
                  -> GHC.Types.Int
                  -> Data.Vector.Fusion.Util.Id GHC.Types.Int)
                   ~#
                 (Main.Updater GHC.Types.Int
                  -> GHC.Types.Int -> GHC.Types.Int -> GHC.Types.Int))

We don't find the actual work in `Main.work01` from dumped
result. It's function body shows that `work01` is calling `cast` with
`Main.work1`, which should doing the actual work.

The dumped core of `work02` looks like this

    Main.work02 [InlPrag=INLINE[0]] :: Main.Work GHC.Types.Int
    [GblId,
     Arity=3,
     Caf=NoCafRefs,
     Str=DmdType LU(L)U(L)m,
     Unf=Unf{Src=Worker=Main.$wwork02, TopLvl=True, Arity=3, Value=True,
             ConLike=True, Cheap=True, Expandable=True,
             Guidance=ALWAYS_IF(unsat_ok=True,boring_ok=False)
             Tmpl= \ (w_s2Uw [Occ=Once] :: Main.Updater GHC.Types.Int)
                     (w1_s2Ux [Occ=Once!] :: GHC.Types.Int)
                     (w2_s2UB [Occ=Once!] :: GHC.Types.Int) ->
                     case w1_s2Ux of _ { GHC.Types.I# ww_s2Uz [Occ=Once] ->
                     case w2_s2UB of _ { GHC.Types.I# ww1_s2UD [Occ=Once] ->
                     case Main.$wwork02 w_s2Uw ww_s2Uz ww1_s2UD
                     of ww2_s2UH { __DEFAULT ->
                     GHC.Types.I# ww2_s2UH
                     }
                     }
                     }}]
    Main.work02 =
      \ (w_s2Uw :: Main.Updater GHC.Types.Int)
        (w1_s2Ux :: GHC.Types.Int)
        (w2_s2UB :: GHC.Types.Int) ->
        case w1_s2Ux of _ { GHC.Types.I# ww_s2Uz ->
        case w2_s2UB of _ { GHC.Types.I# ww1_s2UD ->
        case Main.$wwork02 w_s2Uw ww_s2Uz ww1_s2UD
        of ww2_s2UH { __DEFAULT ->
        GHC.Types.I# ww2_s2UH
        }
        }
        }

    Main.$wwork02
      :: Main.Updater GHC.Types.Int
         -> GHC.Prim.Int# -> GHC.Prim.Int# -> GHC.Prim.Int#
    [GblId,
     Arity=3,
     Caf=NoCafRefs,
     Str=DmdType LLL,
     Unf=Unf{Src=<vanilla>, TopLvl=True, Arity=3, Value=True,
             ConLike=True, Cheap=True, Expandable=True,
             Guidance=IF_ARGS [60 0 0] 152 0}]
    Main.$wwork02 =
      \ (w_s2VX :: Main.Updater GHC.Types.Int)
        (ww_s2W0 :: GHC.Prim.Int#)
        (ww1_s2W4 :: GHC.Prim.Int#) ->
        letrec {
          $wgo1_s2Xg [Occ=LoopBreaker]
            :: GHC.Prim.Int# -> GHC.Prim.Int# -> GHC.Prim.Int#
          [LclId, Arity=2, Str=DmdType LL]
          $wgo1_s2Xg =
            \ (ww2_s2VN :: GHC.Prim.Int#) (ww3_s2VO :: GHC.Prim.Int#) ->
              case GHC.Prim.==# ww2_s2VN ww1_s2W4 of _ {
                GHC.Types.False ->
                  case w_s2VX (GHC.Types.I# ww3_s2VO) of _ { GHC.Types.I# tpl1_B6 ->
                  $wgo1_s2Xg (GHC.Prim.+# ww2_s2VN 1) tpl1_B6
                  };
                GHC.Types.True -> ww3_s2VO
              }; } in
        $wgo1_s2Xg 0 ww_s2W0


After taking a closer look, I realised that `i == m` in guard of
`work02` is carrying `m`, which is passed through the inner functions
found in the core. This lead `$wwork02` to compare `ww2_s2VN` and
`ww1_s2w4`, which both of them passed from argument in `work02`.

Rewriting the guard to compare with constant `0` instead of `m`:

> work03 :: Work Int
> work03 f n0 m = go (P m n0) where
>   {-# INLINE go #-}
>   go !(P i n) | i == 0    = n
>               | otherwise = go (P (i-1) (f n))

New main for taking benchmark:

> main_two :: IO ()
> main_two = do
>   let k = 2 ^ 10
>   defaultMain
>     [ bench "unboxed vector" (nf (work01 add1 0) k)
>     , bench "hand written recursion, take 2" (nf (work03 add1 0) k)
>     ]

Compiling:

    $ ghc -O3 -fllvm -optl-O3 -main-is main_two -o b.out loop.lhs

And running the benchmark:

    benchmarking unboxed vector
    mean: 49.86483 ns, lb 49.84522 ns, ub 49.88565 ns, ci 0.950
    std dev: 103.5967 ps, lb 91.78539 ps, ub 117.8307 ps, ci 0.950

    benchmarking hand written recursion, take 2
    mean: 48.45895 ns, lb 48.37886 ns, ub 48.61787 ns, ci 0.950
    std dev: 554.6886 ps, lb 337.5092 ps, ub 946.0307 ps, ci 0.950

Now the hand written recursion performs about 1.4 nano second faster than
unboxed vector. Whenever possible, compare with constant in guards.
And, if few nano seconds does not matter, just use unboxed vector.