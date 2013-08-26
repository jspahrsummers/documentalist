# Documentalist

Documentalist is an extensible documentation generator that is meant to support
multiple source languages, documentation syntaxes, and output formats.

The project includes both a library and executable component:

 * The **library** is the main focus of development, and will include most of
   the logic. The goal is to support embedded documentation generation
   capabilities in other programs.
 * The **executable** is intended to be a simple front-end that automatically
   hooks up [stages](#stages) for generating pretty documentation output from
   a chosen source language.

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

### Building Concrete Implementations

To make a new implementation for a stage, simply create an instance of the
respective typeclass.

If the implementation is meant to be part of Documentalist proper, make sure to
follow the existing module structure, and add the new module(s) to [the package
description](documentalist.cabal).

## Getting Started

```
brew install --with-clang llvm
cabal install --only-dependencies
cabal configure
cabal build
```

If you want to run the tests:

```
cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal build
cabal test
```
