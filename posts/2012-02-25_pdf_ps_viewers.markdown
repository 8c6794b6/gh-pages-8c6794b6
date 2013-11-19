---
title: Minimalistic pdf and ps viewers for linux - Winter/Spring 2012
author: 8c6794b6
date: February 25, 2012
tags: linux, pdf, ps
description: Looking for a pdf/ps viewer in linux, Feb 2012.
---

I'm reading bunch of pdf and ps files recently. After a system upgrade in
arch linux, [segfault in gv](https://bugs.archlinux.org/task/28516)
happened.  For couple years, I was using
[gv](http://wino.physik.uni-mainz.de/~plass/gv/) for viewing pdf and
postscript files. This segfault made me to dig in to current trends in linux
pdf viewer.

My preferences are:

* Supports pdf and postscript files
* Supports mouse-less control
* Has minimalistic user interface
* Starts quickly
* Use less memory

I took a try to these viewers:

* [apvlv](http://naihe2010.github.com/apvlv/):
  Nice vi-style scroll keybindings, light. But does not support postscript ...
* [epdfviewer](http://trac.emma-soft.com/epdfview/):
   No support for postscript ...
* [evince](http://projects.gnome.org/evince/):
  Supports postscript and pdf. Keybind support for scrolling.
* [mupdf](http://www.mupdf.com/):
  Light, simple interface (shows nothing other than document contents),
  keybind support for scrolling.  But no support for postscript ....
* [zathura](http://pwmt.org/projects/zathura/):
  Minimalistic interface, keybinding support. Scrolling worked smoothly
  than mupdf. Customizable config. Has supports for viewing potscript files
  with [additional plugin](http://pwmt.org/projects/zathura/plugins/).

Those fullfilled my requirement were `evince` and `zathura`. Choosing between
these two, `zathura` met my taste more. Another alternative is `mupdf`, which
uses less memory than `zathura`, but has no supports for postscript files. If
memory usage of zathura become a problem, I'll try
[mupdf plugin](http://pwmt.org/projects/zathura/plugins/), or seek for alternative
again. Until that happen, using `zathura`.

Lessons I learned: sometime it's not a bad thing to get segmentation fault
with package update, if it give opportunity to look for progress in
opensource softwares with same purpose, and find a good alternative.
