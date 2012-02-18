---
title: Adhoc update of ugen parameters
author: 8c6794b6
date: Octover 25, 2011
tags: haskell, supercollider, generic
description: Updating ugen parameters with generics
---
Adhoc update of UGen parameters, with generic functions.

Requires ghc 7.0 or higher:

> {-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}
> {-# LANGUAGE TypeSynonymInstances #-}
>
> module AdhocUpdate where
>
> import Control.Monad
> import Data.Data (Data, Typeable)
> import Data.Generics (everywhere, mkT)
> import Data.List (zipWith4)
> import Control.Concurrent (forkIO, threadDelay)
> import System.Random
>
> import Sound.SC3
> import Sound.SC3.ID
> import Data.Generics.Uniplate.Data (transform, transformM)

To play sound example, run:

    | $ scsynth -u 57110

before invoking OSC sending actions.

We have a synth named `s01`, written with unit generator function from hsc3.

> s01 :: UGen
> s01 =
>   let o = control IR "out" 0
>       p = control KR "pan" 0
>       f = control KR "freq" 440
>       d = control KR "decay" 1
>       e = linen (impulse KR 0.1 0) 1e-2 1 d RemoveSynth
>       s = pan2 (sinOsc AR f 0 * e * 0.3) p 1
>   in  out o s

Actions to make sound with `s01`:

> prepare :: IO ()
> prepare = withSC3 reset
>
> play_s01a :: IO ()
> play_s01a = audition s01

Play it:

    | *AdhocUpdate> parepare >> play_s01a

It makes 440 Hz sine tone, with 1 second decay time, positioning
center.  There are 4 control parameters in s01: out, pan, freq, and
decay.  What we want to do is to use different values for these
parameters.  It is possible to set different value by:

* Send above synthdef to scsynth server
* Wait till the server load the synthdef
* Send `s_new` message to play the synthdef with new parameters

We can send `s_new` message with only those parameters which we want to update.

> play_s01b = withSC3 $ \fd -> do
>   async fd (d_recv $ synthdef "s01" s01)
>   send fd $ s_new "s01" (-1) AddToTail 1 [("freq",3300),("decay",0.3)]

Is there any other way to do this?  We can pass parameters as function
arguments:

> s01' o p f d =
>   let e = linen (impulse KR 0.1 0) 1e-2 1 d RemoveSynth
>       s = pan2 (sinOsc AR f 0 * e * 0.3) p 1
>   in  out o s

But in this manner, we need to specify all values.

> play_s01c = audition $ s01' 0 0 3300 0.3

Can we update specified values only, without sending synthdef, like:

> play_s01d = audition $ ups "freq" 3300 $ ups "decay" 0.3 s01

We can do this with functions in generic modules.

UGen data is defined as:

    | data UGen
    |   = Constant {constantValue :: Double}
    |   | Control {controlOperatingRate :: Rate,
    |              controlName :: String,
    |              controlDefault :: Double,
    |              controlTriggered :: Bool}
    |   | Primitive {ugenRate :: Rate,
    |                ugenName :: String,
    |                ugenInputs :: [UGen],
    |                ugenOutputs :: [Output],
    |                ugenSpecial :: Special,
    |                ugenId :: Int}
    |   | Proxy {proxySource :: UGen, proxyIndex :: Int}
    |   | MCE {mceProxies :: [UGen]}
    |   | MRG {mrgLeft :: UGen, mrgRight :: UGen}

We use `Data` and `Typeable` instances for data types used inside
`UGen` graph. In hsc3-0.9, `UGen` is not defined as instance of Data.
Using `StandaloneDeriving` LANGUAGE pragma to avoid writing boiler plate
codes.

> deriving instance Data UGen
> deriving instance Typeable UGen
> deriving instance Data Rate
> deriving instance Typeable Rate
> deriving instance Data Special
> deriving instance Typeable Special
> deriving instance Data UGenId
> deriving instance Typeable UGenId

We want to update `UGen` with `Control` constructor only, and only if specified
parameter name has matched. This could be done with using `everywhere` from syb.

> ups :: String -> Double -> UGen -> UGen
> ups key value ug = everywhere (mkT f) ug where
>   f (Control r key' _ t) | key == key' = Control r key value t
>   f x                    = x

Play updated sound:

> play_s01e = audition . ups "freq" 3300 . ups "decay" 0.3 $ s01

Or, using `transform` from uniplate package.

> upu :: String -> Double -> UGen -> UGen
> upu key value ug = transform f ug where
>   f (Control r key' _ t) | key == key' = Control r key value t
>   f x                    = x

We can use in same manner as ups.

> play_s01e' = audition . upu "freq" 3300 . upu "decay" 0.3 $ s01

How's its performance? Since we need to traverse synth structure and find out
whether control name matches or not, it should spend more time than
raw messaging style.

Simple grain sound, 1ms decay time sine tone in message sending style:

> play_s01f x =
>   withSC3 . flip send . s_new "s01" (-1) AddToTail 1 $
>   [("freq",x),("decay",0.001)]

And in generic updating style:

> play_s01g x = audition $ upu "freq" x . upu "decay" 0.001 $ s01

Grain generator with 500 nano seconds interval.

> mkgrn k =
>   sequence_ $ map (k >=> const (threadDelay 500)) [10000,9931..0]

Message sending style:

> grn01 :: IO ()
> grn01 = mkgrn play_s01f

Generic update style:

> grn02 :: IO ()
> grn02 = mkgrn play_s01g

Note that, difference is not only coming from parameter update. In `play_s01g`,
entire new synthdef need to be sent to server, this synthdef sending is not so
quick to do in 500 nano seconds.

From performance perspective, message passing style is superior to generic
updating style. This is same in SClang, described in
[NodeMessaging SC help file](http://supercollider.svn.sourceforge.net/viewvc/supercollider/trunk/common/build/Help/ServerArchitecture/NodeMessaging.html).

There's different use cases for generic update.

One is, sending variation of synthdefs. When we know that only fixed variation
of parameters are used for certain synthdef, we can send multiple synthdef
with different default values in advance.
This approach resembles to `variants` concepts, shown in
[SynthDef SC help file](http://supercollider.svn.sourceforge.net/viewvc/supercollider/trunk/common/build/Help/ServerArchitecture/SynthDef.html).

> prepare_s01_variants :: IO ()
> prepare_s01_variants = withSC3 $ \fd ->
>   mapM_ (\(n,u) -> async fd $ d_recv $ synthdef n u)
>     [ ("s01_alpha",upu "pan" 1 $ upu "freq" 880 s01)
>     , ("s01_beta", upu "pan" (-1) $ upu "freq" 1320 s01)
>     , ("s01_gamma",upu "freq" 2640 s01) ]

Parameters in `play_variants_01` are specified in message list,
parameters in `play_variants_02` are predefined in synth
definition. When sending lots of `s_new` messages with fixed value
variations, predefined approach will same the amount of data
transfered to server, since it contains no parameter values in message
body.

> play_variants_01 :: IO ()
> play_variants_01 = withSC3 $ \fd ->
>   let ms = [ [("freq",880),("pan",1)]
>            , [("freq", 1320),("pan",-1)]
>            , [("freq",2640)]
>            , [("freq", 1320),("pan",-1)]
>            , [("freq",2640)]
>            , [("freq",880),("pan",1)] ]
>   in  forM_ ms $ \m ->
>         send fd (s_new "s01" (-1) AddToTail 1 m) >> threadDelay (5*10^5)
>
> play_variants_02 :: IO ()
> play_variants_02 = withSC3 $ \fd ->
>   let ds = [ "s01_alpha", "s01_beta", "s01_gamma"
>            , "s01_beta", "s01_gamma", "s01_alpha" ]
>   in  forM_ ds $ \d ->
>         send fd (s_new d (-1) AddToTail 1 []) >> threadDelay (5*10^5)

Another usage of generic update approach is, `UGen` replacing.

There are couple oscillator functions with type
'Rate -> UGen -> UGen -> UGen':

    | sinOsc :: Rate -> UGen -> UGen -> UGen
    | lfCub :: Rate -> UGen -> UGen -> UGen
    | lfPar :: Rate -> UGen -> UGen -> UGen
    | lfSaw :: Rate -> UGen -> UGen -> UGen
    | lfTri :: Rate -> UGen -> UGen -> UGen

We can write a function taking these oscillator:

> mks02 o =
>  let s = pan2 (o AR f 0 * e * 0.3) p 1
>      e = envGen KR 1 1 0 1 RemoveSynth (envPerc 1e-2 1)
>      f = control KR "freq" 440
>      p = control KR "pan" 0
>  in  out 0 s

and play it:

> play_s02_c = audition $ mks02 lfCub
> play_s02_t = audition $ mks02 lfTri
> play_s02_s = audition $ mks02 lfSaw

We can do similar thing without passing function as an
argument. `UGen` functions `sinOsc`, `lfCub`, `lfPar`,
`lfSaw`, `lfTri` are defined with `Primitive` constructor:

    | *AdhocUpdate> sinOsc AR 440 0
    | Primitive { ugenRate = AR
    |           , ugenName = "SinOsc"
    |           , ugenInputs = [Constant {constantValue = 4400.0}
    |                          ,Constant {constantValue = 0.0}]
    |           , ugenOutputs = [AR], ugenSpecial = Special 0, ugenId = -1}

A function to update `Primitive`:

> primu :: String -> String -> UGen -> UGen
> primu from to ug = transform f ug where
>   f p@(Primitive r n is os s idx) | n == from = p {ugenName = to}
>   f x = x

Using `primu`, we can update `sinOsc` used in `s01` to different
oscillator.

> set_osc_s01 :: String -> UGen
> set_osc_s01 = flip (primu "SinOsc") s01
>
> play_s01_cub, play_s01_tri, play_s01_saw :: IO ()
> play_s01_cub = audition $ set_osc_s01 "LFCub"
> play_s01_tri = audition $ set_osc_s01 "LFTri"
> play_s01_saw = audition $ set_osc_s01 "LFSaw"

By using `String` to specify `UGen` name, we loosed type safety. We
can have chance to send a `UGen` graph that does not work.

> s01_ng1 = set_osc_s01 "NoSuchUnitGenerator"

Invoking `audition s01_ng1` will show an error message in scsynth:

    exception in GraphDef_Recv: UGen 'NoSuchUnitGenerator' not installed.

There's another chance to send malformed UGen graph, to update a ugen
with different inputs. `UGen` function `phasor` has type:

    | phasor :: Rate -> UGen -> UGen -> UGen -> UGen -> UGen -> UGen

Its type is different from `sinOsc`:

    | sinOsc :: Rate -> UGen -> UGen -> UGen

Though, we can update `"SinOsc"` with `"Phasor"`.

> s01_ng2 = primu "SinOsc" "Phasor" s01

Again, this likely to give us unintended result. Is there any safer
way to do this?

We can pass oscillator function instead of `String` to get the name of
`UGen`. `"LFCub"` string literal used in above `play_s01_cub` was picked
up from this result:

    | *AdhocUpdate> lfCub AR 440 0

Let us call this type as `OscUG`.

> type OscUG = Rate -> UGen -> UGen -> UGen

Rewrite `set_osc_s01` to:

> set_osc_s01' :: OscUG -> UGen
> set_osc_s01' o = primu "SinOsc" (oscName o) s01
>
> oscName :: OscUG -> String
> oscName o = ugenName (o AR 0 0)

Note the use of dummy arguments `AR`, `0`, and `0`.  Using `oscName`,
`OscUG` could be shown as `String`. Make it an instance of `Show`
class.

> instance Show OscUG where
>   show = oscName

We can write a function to swap all occurance of `OscUG` s.

> swapOsc :: OscUG -> OscUG -> UGen -> UGen
> swapOsc from to ug = primu (oscName from) (oscName to) ug

Below will swap all `sinOsc` to `lfSaw` in `s01`.

> play_s01_saw' :: IO ()
> play_s01_saw'= audition $ swapOsc sinOsc lfSaw s01

Example usage of `swapOsc`, randomly replacing `sinOsc`.

> oscUGs :: [OscUG]
> oscUGs = [sinOsc,fSinOsc,lfPar,lfCub,lfTri,lfSaw]
>
> randomOscUG :: RandomGen g => g -> (OscUG, g)
> randomOscUG g = case randomR (0, length oscUGs - 1) g of
>     (i,g') -> (oscUGs !! i, g')

`randomOscUG` will randomly return oscillators:

    | *AdhocUpdate> replicateM 5 (getStdRandom randomOscUG)
    | [LFPar,FSinOsc,LFSaw,FSinOsc,LFPar]

Make a synth containing 30 sinOscs.

> s03 :: Double -> UGen
> s03 ff =
>   let mko f a d = sinOsc AR (mkf f) 0 * (mke a d) * 0.01
>       mkf f = constant f
>       mke a d = envGen KR 1 1 0 1 DoNothing $ envSine a d
>       me = envGen KR 1 0 1 1 RemoveSynth $ envLinen 0 5 2.5 1
>       os = zipWith3 mko
>            [x*1.2081 | x <- [ff,ff*2..]]
>            [1.25**x  | x <- [1,2..]]
>            [2.5*x    | x <- [10,9..]]
>       sig = (mix $ mce $ take 30 os) * me
>   in  out 0 (pan2 sig 0 1)

And a function to randomly replace `sinOsc` with `getStdRandom`.

> randomizeOsc :: UGen -> IO UGen
> randomizeOsc ug = transformM k ug where
>   k (Primitive r n is os s j)
>     | n == "SinOsc" = getStdRandom randomOscUG >>= \o ->
>       return $ Primitive r (oscName o) is os s j
>   k x = return x

Takes fundamental frequency and play sum of oscillators. Oscillators
used for each partial are choosed randomly.

> play_s03_rand :: Double -> IO ()
> play_s03_rand x = audition =<< randomizeOsc (s03 x)

Invoking:

    | *AdhocUpdate> play_s03_rand 30

will build a UGen graph with randomly chosen oscillator UGen for each partial.

Variation without using generic update. Type signature of this synth is
`IO UGen`, since its callling `randomOscUG` inside.

> s03' :: Double -> IO UGen
> s03' ff = do
>   ougs <- replicateM 30 (getStdRandom randomOscUG)
>   let mko o f a d = o AR (mkf f) 0 * (mke a d) * 0.01
>       mkf f = constant f
>       mke a d = envGen KR 1 1 0 1 DoNothing $ envSine a d
>       me = envGen KR 1 0 1 1 RemoveSynth $ envLinen 0 5 2.5 1
>       os = zipWith4 mko
>            ougs
>            [x*1.2081 | x <- [ff,ff*2..]]
>            [1.25**x  | x <- [1,2..]]
>            [2.5*x    | x <- [10,9..]]
>       sig = (mix $ mce os) * me
>   return $ out 0 (pan2 sig 0 1)

The result UGen graph is different from `s03`. Since `s03` may replaces oscillator
used for left and right of same frequency with different oscillators.  s03'
uses same oscillator for left and right of same frequency.

> play_s03_rand' :: Double -> IO ()
> play_s03_rand' x = audition =<< s03' x