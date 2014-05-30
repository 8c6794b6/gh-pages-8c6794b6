---
title: Modifying commit attributes and editing hunk in git
author: 8c6794b6
data: November 28, 2013
description: Two git tips for modifying existing commits and editing hunk.
tags: git
---

**Changing author (or any other commit attribute) in existing commits**

From
[this SOF Q&A](http://stackoverflow.com/questions/750172/how-do-i-change-the-author-of-a-commit-in-git). Linked
page was showing a scenario to change committer and author's name and email from
old value to new value:


    git filter-branch --commit-filter '
        if [ "$GIT_COMMITTER_NAME" = "<OLD_NAME>" ];
        then
            GIT_COMMITTER_NAME="<NEW_NAME>";

            # ...
            # modify other attributes if any, then...

            git commit-tree "$@";
        else
            git commit-tree "$@";
        fi' HEAD

Comment in the answer was mentioning `git env-filter` will change all commits,
but the one shown above allows conditional behaviour.


**Editing hunk while adding commits**

There are
[couple](http://joaquin.windmuller.ca/post/35-selectively-select-changes-to-commit-with-git-or-imma-edit-your-hunk)
[articles](http://pivotallabs.com/git-add-e/) mentioning it, including the
[git add man page](http://git-scm.com/docs/git-add). Section `EDITING PATCHES`
in the manual has detailed explanation.
