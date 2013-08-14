# Documentalist

Documentalist is an extensible documentation generator that is meant to support
multiple source languages, documentation syntaxes, and output formats.

## Stages

The architecture is split into three major stages.

 1. A [SourceParser](Text/Documentalist/SourceParser.hs) locates documentation
    comments and the declarations to which they're attached. Currently, there's
    only one `SourceParser`, built on
    [LibClang](http://clang.llvm.org/docs/Tooling.html).
 1. A [CommentParser](Text/Documentalist/CommentParser.hs) interprets
    declarations and comment texts according to a predefined syntax, and creates
    a language-independent AST. The only `CommentParser` currently planned will
    interpret the [TomDoc](http://tomdoc.org) format.
 1. A [Writer](Text/Documentalist/Writer.hs) generates pretty output, like the
    final result of [Doxygen](http://www.stack.nl/~dimitri/doxygen/) or
    [Appledoc](http://gentlebytes.com/appledoc/).

Each stage should depend only on the previous, such that the `CommentParser` has
no knowledge of the source language, and the `Writer` has no knowledge of the
documentation syntax or source language.

## Getting Started

```
brew install llvm
cabal configure
cabal build
```
