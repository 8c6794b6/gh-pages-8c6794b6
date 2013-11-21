---
title: Sound of lena
author: 8c6794b6
date: December 1, 2011
tags: haskell, sound, image, repa
description: Converting image with repa and sndfile
---
The idea of making sound from images was there from quite a long time ago.

For instance, using a picture of probably
[the most famouse lady in image processing](http://en.wikipedia.org/wiki/Lenna)
as input:

<img style="display:block; margin: 25px auto;"
     src="http://github.com/8c6794b6/spectrofy/raw/gh-pages/lena_in.bmp" />

to get a sound with this spectrogram:

<img style="display:block; margin: 25px auto"
     src="http://github.com/8c6794b6/spectrofy/raw/gh-pages/lena_out.png" />

No idea what human beings in stone ages were thinking, but in the ages of
computers, Iannis Xenakis was already thinking about the use of images as
input data for making audio, in 1970s, in a system called
[UPIC](http://en.wikipedia.org/wiki/UPIC).
So far I know, couple commercial software exist for editing sound data
in frequency domain via graphical user interface. Like
[metasynth](http://www.uisoftware.com/MetaSynth/index.php) and
[photosounder](http://photosounder.com),
to name a few. One use of this technique I know is in
[Windowlicker](http://en.wikipedia.org/wiki/Windowlicker),
by Aphex Twin.


There is
[a nice article](http://www.devrand.org/2013/04/image-to-spectrogram.html)
which describing about making spectrogram of image with perl,
it inspired me to do it with haskell. The code used to convert above image
was implemented in haskell, with
[repa](http://repa.ouroborus.net/),
[fftw](http://www.fftw.org/), and
[libsndfile](http://www.mega-nerd.com/libsndfile/).

Source code of executable, named _spectrofy_, could be found
[here](http://github.com/8c6794b6/spectrofy).
Couple packages that I packed as repa interface are also available.
The BMP image of lena was converted to wav audio, and then spectrogram
was created with sox:

    $ spectrofy fft -f512 lena.bmp lena.wav
    $ sox lena.wav -n spectrogram -x 256 -y 257 -Z -48 -z 45

The spectrogram shown above was created with above commands. Not with
[photoshop](http://www.gimp.org).

----

Update: 2013-11-21 - modified link destination of images and spectrogram
article.
