{-# LANGUAGE QuasiQuotes #-}
-- cd Tests; ghci TomDocExamples.hs -i..
module TomDocExamples where

import TestHelpers

test = [docblock|
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
