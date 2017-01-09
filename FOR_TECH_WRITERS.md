Documenting IOHK Projects Using Readthedocs
===

Abstract
---

In this document we talk about documenting IOHK projects such as CSL
using `mkdocs`, a tool compatible with Readthedocs platform. Here we
address issues of documentation upkeep as well as per-release
documentation generation.

Introduction
---

The matter of documentation upkeep, especially targeting end-users is one
of the most challenging tasks in software maintenance. Here we propose a
method of making this upkeep as painless as possible, making sure that
documentation (targeting end-users or fellow developers) is up to date.

Readthedocs/GitHub-compatible Commentaries in Markdown
---

First of all, it's important to note that even though Markdown is used
to document IOHK projects, one can insert one-line commentaries using
the following label hack:

```
[//]: # (Commentary goes here)
```

Those commentaries are internal and won't be displayed at all in the
rendered documentation (see
[current
version](http://pos-haskell-prototype.readthedocs.io/en/latest/) of
Cardano SL documentation for a reference. In the documentation index,
you can't see the commentaries for developers that refer to this file,
while in the Markdown text they [exist]()

Pushing Incomplete Documentation
---

Subject to Change Marker
---

Sprint/Feature Branch-Related Changes
---

Feedback Request Marker
---

Documentation Report Scripts
---


