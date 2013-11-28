---
title: French input methods and œ
author: 8c6794b6
date: November 28, 2013
tags: emacs, french
---

Notes about typing français in emacs with English keyboard.

Change input method with `C-x RET C-\` or `M-x set-input-method`. There are
three french input methods for English keyboard: `french-prefix`,
`french-postfix`, and `french-alt-postfix`. `french-prefix` input method will
let the accent to be typed before alphabets, e.g: to type `ç`, type `,c`. In
`french-postfix` input method, type `c,`. Help of `french-prefix` input method,
which shown with `C-h I` or `M-x describe-input-method`, had a nice table:


    Input method: french-prefix (mode line indicator:FR>)

    French (Français) input method with prefix modifiers

        effect   | prefix | examples
     ------------+--------+----------
        acute    |   '    | 'e -> é
        grave    |   `    | `a -> à
      circumflex |   ^    | ^a -> â
      diaeresis  |   "    | "i -> ï
       cedilla   | ~ or , | ~c -> ç   ,c -> ç
       symbol    |   ~    | ~> -> »   ~< -> «

Characters with accents were able to type with these two modes, though the
character `œ` was not. Found an entry in emacs-devel thread posted in 2008 which
mentioning [french ç and œ][1]. To type in œ, use `latin-postfix` or
`latin-prefix` input method, the key sequence to type `œ` is `o/2` and `/o2`,
respectively. `latin-prefix` and `latin-postfix` input methods contains those
characters typed in with french input methods:

    Input method: latin-postfix (mode line indicator:L<)

    Latin character input method with postfix modifiers.
    This is the union of various input methods originally made for input
    of characters from a single Latin-N charset.

                 | postfix | examples
     ------------+---------+----------
      acute      |    '    | a' -> á
      grave      |    `    | a` -> à
      circumflex |    ^    | a^ -> â
      diaeresis  |    "    | a" -> ä
      tilde      |    ~    | a~ -> ã
      cedilla    |    ,    | c, -> ç
      ogonek     |    ,    | a, -> ą
      breve      |    ~    | a~ -> ă
      caron      |    ~    | c~ -> č
      dbl. acute |    :    | o: -> ő
      ring       |    .    | u. -> ů
      dot        |    .    | z. -> ż
      stroke     |    /    | d/ -> đ
      nordic     |    /    | d/ -> ð   t/ -> þ   a/ -> å   e/ -> æ   o/ -> ø
      others     |    /    | s/ -> ß   ?/ -> ¿   !/ -> ¡   // -> °
                 | various | << -> «   >> -> »   o_ -> º   a_ -> ª

    Doubling the postfix separates the letter and postfix: e.g. a'' -> a'


[1]: http://lists.gnu.org/archive/html/emacs-devel/2008-10/msg00760.html
