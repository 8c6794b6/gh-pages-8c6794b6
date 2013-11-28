---
title: Making sound with side effects
author: 8c6794b6
date: November 13, 2011
tags: haskell, supercollider, sound
description: Making sound with side effects of fibonacci sequence
---
This post is a scratchy idea for making sound with side
effects. Sample recording created with side effects from fibonacci sequence
sounds like this:

<audio controls="controls" src="/audio/fib.ogg"
       style="display:block; margin: 10px auto;">
</audio>

During debug, we print out intermediate data, as side effects. Like this, can
we make sounds from those intermediate data, as side effects?

> {-# LANGUAGE GeneralizedNewtypeDeriving #-}
> module SideEffectSound where

We will use `scsynth` as sound synthesis engine.

> import Control.Concurrent
> import Control.Monad
> import Control.Monad.Reader
> import Control.Monad.State
> import Control.Monad.Writer
> import Data.Word
> import Sound.OpenSoundControl
> import Sound.SC3
> import Sound.SC3.ID

As an action to get side effect, using fibonacci sequence here. We will not
use memoizatoin nor any other method to achive efficiency, since our concern
resides in those side effects.

> fib_visible :: Int -> IO Int
> fib_visible n
>   | n == 0    = putStr "0" >> return 1
>   | n == 1    = putStr "1" >> return 1
>   | otherwise = do
>       putStr (show n)
>       a <- fib_visible (n-1)
>       b <- fib_visible (n-2)
>       return $ a + b

Sample output:

    | ghci> fib_visible 5
    | 5432101210321018

Like above, we will make sound from side effect with fibonacci sequence.
Using a newtype wrapper for sending OSC message to default scsynth, running in
UDP port 57110 of localhost (assuming that scsynth is running with this
default settings).

> newtype Sound a = Sound {unSound :: ReaderT UDP IO a}
>   deriving (Functor, Monad, MonadReader UDP, MonadIO)
>
> runSound :: Sound a -> IO a
> runSound = withSC3 . runReaderT . unSound

With using `Sound` wrapper, we can write audible fibonacci sequence with
side effect pretty much resembling to its visible variant. The only difference
we see in the body of code is use of `mkSound` instead of `putStr`.

> fib_audible :: Int -> Sound Int
> fib_audible n
>   | n == 0    = mkSound 0 >> return 1
>   | n == 1    = mkSound 1 >> return 1
>   | otherwise = do
>       mkSound n
>       a <- fib_audible (n-1)
>       b <- fib_audible (n-2)
>       return $ a + b

We could write `fib_audible` and `fib_visible` with using `mkFib`.

> mkFib :: Monad m => (Int -> m ()) -> Int -> m Int
> mkFib act n
>   | n == 0    = act 0 >> return 1
>   | n == 1    = act 1 >> return 1
>   | otherwise = do
>       act n
>       a <- mkFib act (n-1)
>       b <- mkFib act (n-2)
>       return $ a + b
>
> fib_visible' = mkFib (putStr . show)
> fib_audible' = mkFib mkSound

The sound maker. Sends OSC message to scsynth and pause. Will not make sound
when n is 0.

> mkSound :: Int -> Sound ()
> mkSound n
>   | n == 0    = liftIO $ threadDelay timeUnit
>   | otherwise = do
>     fd <- ask
>     let n' = fromIntegral n
>     liftIO $ do
>       send fd $ s_new "fibdef" (-1) AddToHead 1
>         [ ("freq", n' * 220)
>         , ("amp", 0.05 + (0.01 * n'))
>         , ("pan", (-1) + (2/3) * fromIntegral (n `mod` 4))
>         , ("decay", 0.1 + (n' * 0.175)) ]
>       threadDelay timeUnit

Time interval used for pausing.

> timeUnit :: Int
> timeUnit = 12800

Synthdefs used in this piece. Simple synthdef taking `freq`, `pan` and
`amp` parameter.

> fibdef :: UGen
> fibdef =
>   let sig = sinOsc AR frq 0 * e * amp
>       frq = control KR "freq" 440
>       amp = control KR "amp" 0.1
>       e   = envGen KR 1 1 0 1 RemoveSynth shp
>       shp = envPerc 1e-4 dcy
>       pan = control KR "pan" 0
>       dcy = control KR "decay" 0.5
>   in  out 0 (pan2 sig pan 1)

Send the synthdef to scsynth.

> setup_fib :: Transport t => t -> IO OSC
> setup_fib fd = do
>   async fd $ bundle immediately
>     [ d_recv $ synthdef "fibdef" fibdef ]

Now its ready to hear the sound.

> play_fib_sound :: IO Int
> play_fib_sound = do
>   withSC3 $ \fd -> reset fd >> setup_fib fd
>   runSound $ fib_audible 17

In some case, Instead of listening sound in real time, we might want to write
score and render the result in batch command. Below newtype `Score` will do
this job. `Score` is a combination of Reader, Writer, and State monad, it
takes delta time as Reader monad's environment value, used to increment
current time in score file. Writer accumulate the OSC list written to score
file, and State monad hold the current time in the score. Alternatively, it
could be written with RWS monad, or single State monad. The use of newtype
wrapper with `GeneralizedNewtypeDeriving` here is, I suppose, just a matter of
taste.

> newtype Score a =
>   Score {unScore :: ReaderT Word64 (WriterT [OSC] (State Word64)) a} deriving
>   (Functor, Monad, MonadState Word64, MonadWriter [OSC], MonadReader Word64)
>
> runScore :: Score a -> Word64 -> [OSC]
> runScore m delta =
>   evalState (execWriterT (runReaderT (unScore m) delta)) 0

Action to make score from each data passed in fibonacci, and fibonacci score
writer monad. We can use `mkFib` to make a new action to write score from side
effects generated from fibonacci action.

> mkScore :: Int -> Score ()
> mkScore n = do
>   delta <- ask
>   t0 <- get
>   let n' = fromIntegral n
>   tell $ [ bundle (NTPi t0)
>     [ s_new "fibdef" (-1) AddToTail 1
>       [ ("freq", n' * 220)
>       , ("amp",  0.05 + (n' * 0.02))
>       , ("pan", (-1) + (2/3) * fromIntegral (n `mod` 4))
>       , ("decay", 0.1 + (n' * 0.25)) ]]]
>   put (t0 + delta)
>
> fib_score :: Int -> Score Int
> fib_score = mkFib mkScore

Function to write score file, with initial OSC message to send synthdef and
add default group to scsynth.

> write_fib_score :: FilePath -> Score a -> IO ()
> write_fib_score path m = do
>   let iosc = bundle (NTPi 0)
>         [ g_new [(1,AddToTail,0)]
>         , d_recv $ synthdef "fibdef" fibdef ]
>   writeNRT path (iosc : runScore m (fromIntegral timeUnit * 10000))

Test it.

> test_fib_score :: IO ()
> test_fib_score = write_fib_score "/tmp/fib.osc" (fib_score 12)

Attached recording in the beginning of this post was created from `fib.osc`,
fed to scsynth in non-realtime mode.

    | $ scsynth -o 2 -N /tmp/fib.osc _ out.wav 48000 WAVE float
