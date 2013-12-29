{-# LANGUAGE QuasiQuotes #-}
-- cd Tests; ghci TomDocExamples.hs -i..
module TomDocExamples where

import TestHelpers
import Test.HUnit
import Text.Documentalist.Types.DocBlock
import Text.Documentalist.CommentParser

testsGo = runTestTT tests
tests = test [ "codetest" ~: codetest ~=? codetest_ref ]

codetest_ref :: Either CommentParseException DocBlock
codetest_ref = Right (DocBlock {summary = CodeBlock (Code "\n`1 + 1`\n"), description = [], parameters = [], example = Nothing, result = Nothing})

codetest = [docblock|
`1 + 1`
|]

testdoc = [docblock|
whats this

*stuff* _goes_ **here** and is __cool__

`1 + 1`

```
let code = good
in code
```

> A quote from someone
> who likes to use computers
> > i like computers
> > they gud
> quote was 4/10

normal mode text
paragraph

  a bit indented
  ya kno
  but not a quote
  this is ~~fancy~~


[wow](http://github.com)
![gifs](http://i.imgur.com/vwMin.gif)
|]
