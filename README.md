# documentalist

The architecture should eventually look something like this:

 1. A **source language parser** (like Clang) locates documentation comments and the declarations to which they're attached.
 1. The declarations and raw comment texts are passed to a **comment parser**, which breaks comments down into semantic sections and formatting instructions, eventually creating a language-independent AST.
 1. A **writer** is used to translate the AST into a specific output format.
