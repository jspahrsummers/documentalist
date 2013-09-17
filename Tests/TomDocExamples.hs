{-# LANGUAGE QuasiQuotes #-}
module TomDocExamples where

import TestHelpers

test = [docblock|
whats this

*stuff* _goes_ **here** and is __cool__

`1 + 1`

[wow](http://github.com)
![gifs](http://i.imgur.com/vwMin.gif)
|]
