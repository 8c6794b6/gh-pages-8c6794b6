---
title: Updating gh-pages
author: 8c6794b6
date: November 19, 2013
tags: hakyll, haskell, git
description: Made an update to gh-pages
---

Made couple updates to this gh-pages.

Migrated to hakyll 4. Had a thought for doing this but haven't done for
... perhaps more than a year. As
[already](http://jaspervdj.be/hakyll/tutorials/hakyll-3-to-hakyll4-migration-guide.html)
[mentioned](http://blog.clement.delafargue.name/posts/2013-01-17-hakyll-4.html),
`Compiler` is not an instance of `Arrow`, instead an instance of `Monad`, `Page`
and MetaCompilers have been removed, and couple more changes.

Replaced "8c6794b6.github.com" to "8c6794b6.github.io", since URLs for gh-pages
have changed. According to
[User, Organization and Project Pages](https://help.github.com/articles/user-organization-and-project-pages)
from GitHub Help, repository with `*.github.io` is the latest naming scheme.

Update [404 page](/404.html), showing bigger lambda face now.
