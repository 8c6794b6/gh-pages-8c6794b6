--
title: Tip using 'asTypeOf'
author: 8c6794b6
date: Oct 11, 2012
tags: haskell, tips
description: A tip using 'asTypeOf' to fix types
--

I found myself an idiot for such a long time I was not aware of this
use of `asTypeOf` function.

> module AsTypeOf where

When we have a type with multiple varialbles, e.g. `Either`:

> rightChar = Right 'a'

In ghci:

    ghci> :t rightChar
    rightChar :: Either a Char

We are yet not sure for remaining type variables. In above case, we
still do not have a clue to fix the type of `Left` constructor.

One way to fix the type used for `Left` is wrapping multiple values in
single list:

> rightCharInList = [rightChar, Left True]

Showing the type:

    ghci> :t rightCharInList
    rightCharInList :: [Either Bool Char]

Or another, using `asTypeOf` instead of list:

> rightChar' = rightChar `asTypeOf` Left True

Now the type for `Left` constructor is fixed, value is identical to
`rightChar`:

    ghci> rightChar'
    Right 'a'
    ghci> :t rightChar'
    rightChar' :: Either Bool Char
