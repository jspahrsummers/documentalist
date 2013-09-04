{-# LANGUAGE QuasiQuotes #-}
module Tests.TomDocExamples where

import Tests.TestHelpers

test = [docblock|
whats this

*stuff* _goes_ **here** and is __cool__

`1 + 1`

[wow](http://github.com)
![gifs](http://i.imgur.com/vwMin.gif)
|]
