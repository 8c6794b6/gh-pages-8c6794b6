---
title: Bytestring and unicode chars from U+0080 to U+00FF
author: 8c6794b6
date: November 2, 2011
tags: haskell, utf-8, web
description: Using bytestring data between U+0080 and U+00FF
---

While I was viewing html files, got an error:

    | A web handler threw an exception. Details:
    | Cannot decode byte '\xa0': Data.Text.Encoding.Fusion.streamUtf8: Invalid UTF-8 stream

The html file was served by [snap](http://www.snapframework.com). Somewhere
inside in it's data transfer, something wrong is happening with utf8 decoding
with function in
[Data.Text](http://http://hackage.haskell.org/packages/archive/text/latest/doc/html/Data-Text.html).
The byte failed to decode: `'\xa0'`, was
[no-break space](http://www.fileformat.info/info/unicode/char/a0/index.htm).


No-break space in UTF-8 is `0xc2 0xa0`, expressed with 2 bytes.
When `decodeUtf8` convert `ByteString`, it is expecting `0xc2` prefix to be passed
together, but when above error occured, only the latter `0xa0` has passed.
Later I realised that all characters in between unicode code point from U+0080
to U+00FF will raise similar exception, since those non-ascii characters need
explicit `0xc2` or `0xc3` prefix.

How can we get rid of this exception?

> module Main where
>
> import Data.Word (Word8)
> import qualified Data.ByteString as B
> import qualified Data.ByteString.Char8 as C8
> import qualified Data.ByteString.UTF8 as U8
> import qualified Data.Text as T
> import qualified Data.Text.IO as T
> import qualified Data.Text.Encoding as E

Suppose that we have latin1 string:

> latin1s :: String
> latin1s = "français"

Applying `decodeUtf8` to latin1 chars will raise exception, as shown in top of
this writing.

> printNG :: String -> IO ()
> printNG ss = T.putStrLn $ E.decodeUtf8 $ C8.pack ss

Results in:

    | *Main> ptintNG latin1s
    | "*** Exception: Cannot decode byte '\x61': Data.Text.Encoding.decodeUtf8: Invalid UTF-8 stream

The problem in this case was that `ByteString` representation of `ç` is
expressed with single byte, instead of 2 bytes.

When the characters we matter are ascii and latin1 only, simple solution is to
pad the prefix.

> padLatin1s :: C8.ByteString -> T.Text
> padLatin1s = E.decodeUtf8 . C8.foldr f C8.empty where
>   f c a | '\128' <= c && c < '\192' = C8.cons '\194' . C8.cons c $ a
>         | '\192' <= c && c < '\256' =
>           C8.cons '\195' . C8.cons (toEnum (fromEnum c - 0x40)) $ a
>         | otherwise = C8.cons c a

Now we can decode bytestring data to Text data without exception.

> printPadded :: String -> IO ()
> printPadded ss = T.putStrLn $ padLatin1s $ C8.pack ss

Results in:

    | *Main> printPadded latin1s
    | français

Though above padding approach does not work when documents has mixed use of
characters between U+0080 and U+00FF with non-ascii, non-latin1 characters,
since its always padded. Though it may useful when converting a limited set of
character, which could expressed with sequence of hex numbers from `0x00` to `0xff`.
Those inputs using hex numbers above `0xff` are not representable with
`Data.ByteString`.

> zhongwen :: String
> zhongwen = "中文"

Padding does not make sense:

    | *Main> printPadded zhongwen
    | -

Which function made the change?  When we use `pack` from
`Data.ByteString.Char8` to convert this String to ByteString, it results in:

    | *Main> C8.pack zhongwen
    | "-\135"

Which is not what we want in most cases.

Using pack from `Data.ByteString.Char8` will truncate hex number used for
the characters, we get a value which could be expressed in `Word8` only.

Representing with ByteString is possible, however. It needs a longer,
             proper sequence of hex numbers.

> zhongwen' :: [Word8]
> zhongwen' = [0xe4, 0xb8, 0xad, 0xe6, 0x96, 0x87]

We use Data.ByteString.pack to convert list of hex numbers:

> printZhongwen :: IO ()
> printZhongwen = T.putStrLn $ E.decodeUtf8 $ B.pack zhongwen'

Shows:

    | *Main> printZhongwen
    | 中文

So what we want here is a utf-8 aware function that converts list of `Char`
to `ByteString`. A package
[utf8-string](http://hackage.haskell.org/package/utf8-string-0.3) has a
converting function from `String` to `ByteString`.

> printUTF8 :: String -> IO ()
> printUTF8 ss = T.putStrLn $ E.decodeUtf8 $ U8.fromString ss

Results:

    | *Main> printUTF8 latin1s
    | français
    | *Main> printUTF8 zhongwen
    | 中文

There should be a situation with more complicated character encoding issues.
To get more support of unicode, using
[text-icu](http://hackage.haskell.org/package/text-icu) should be better, as
suggested in haddock comment of [text](http://hackage.haskell.org/package/text).
