module Test.Main where

import Prelude
import Effect (Effect)
import Data.Maybe(Maybe(..))
import Math  (round)
import Partial.Unsafe(unsafePartial)
import Graphics.Canvas.Geometry (abs, circle
                         , halfline, length, line, meets, middle
                         , normalTo, ord, point, rename
                         , segment, vector)

foreign import assert :: String -> Boolean -> Effect Unit

main :: Effect Unit
main = unsafePartial $ do
  let a = point "A" 310.0 320.0
  let b = point "B" 100.0 210.0
  let c = circle a (length $ vector a b)
  let n = normalTo $ vector a b
  let d = halfline a n
  let [e] = (rename "E") <$> (d `meets` c)
  let eb = segment e b Nothing
  let i = middle "I" eb
  let [f] = (rename "F") <$> c `meets` (halfline a (vector b a))
  let g = circle f (length $ vector i e)
  let [h1,h2] = g `meets` c
  let [h] = (rename "H") <$> (line a e) `meets` (line i f)
  -- The following line is useless but valid:
  let [] = (segment b h Nothing) `meets` (segment e f Nothing)
  let [j] = (rename "J") <$> (halfline b (vector b h)) `meets` 
                              (segment e f Nothing)
  assert "computations OK" $ abs j == 470.0 && round(ord j) == 270.0
  pure unit
  
