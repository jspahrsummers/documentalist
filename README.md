![I have no idea what I'm doing](https://i.chzbgr.com/maxW500/6675441920/h9395C28A/)

The architecture should eventually look something like this:

 1. A **source language parser** (like Clang) locates documentation comments and the declarations to which they're attached.
 1. The declarations and raw comment texts are passed to a **comment parser**, which breaks comments down into semantic sections and formatting instructions, eventually creating a [Pandoc][]. Unfortunately, it will probably not be possible to build a real Pandoc reader, since our input is more structured than a plain string.
 1. A **[Pandoc][] writer** is used to generate pretty output.

[Pandoc]: http://hackage.haskell.org/package/pandoc
