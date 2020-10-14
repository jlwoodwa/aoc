{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Day 3: Crossed Wires
module Y2019.D03 (d03a) where

import Control.Exception (assert)
import Control.Monad (join)
import Data.Bifunctor (bimap)
import Data.Foldable (minimum)
import Data.Function (on)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S
import Data.Text (Text, lines, null, splitOn, uncons)
import Data.Text.Read (decimal)
import Data.Tuple (swap)
import Debug.Trace (traceShowId)
import Numeric.Natural (Natural)
import Prelude hiding (lines, null)

-- | The gravity assist was successful, and you're well on your way to the Venus
-- refuelling station. During the rush back on Earth, the fuel management system
-- wasn't completely installed, so that's next on the priority list.
--
-- Opening the front panel reveals a jumble of wires. Specifically,
-- __two wires__ are connected to a central port and extend outward on a grid.
-- You trace the path each wire takes as it leaves the central port, one wire
-- per line of text (your puzzle input).
--
-- The wires twist and turn, but the two wires occasionally cross paths. To fix
-- the circuit, you need to
-- __find the intersection point closest to the central port__. Because the
-- wires are on a grid, use the
-- <https://en.wikipedia.org/wiki/Taxicab_geometry Manhattan distance> for this
-- measurement. While the wires do technically cross right at the central port
-- where they both start, this point does not count, nor does a wire count as
-- crossing with itself.
--
-- For example, if the first wire's path is @R8,U5,L5,D3@, then starting from
-- the central port (@o@), it goes right @8@, up @5@, left @5@, and finally down
-- @3@:
--
-- @
-- ...........
-- ...........
-- ...........
-- ....+----+.
-- ....|....|.
-- ....|....|.
-- ....|....|.
-- .........|.
-- .o-------+.
-- ...........
-- @
d03a :: Text -> Natural
d03a =
  fromIntegral
    . minimum
    . map (uncurry ((+) `on` abs))
    . S.toList
    . S.delete (0, 0)
    . uncurry S.intersection
    . both
      ( S.fromList
          . scanl (uncurry (bimap `on` (+))) (0, 0)
          . join
          . map
            ( uncurry replicate
                . swap
                . bimap
                  dired
                  ( uncurry (flip (assert . null))
                      . either error id
                      . decimal
                  )
                . fromMaybe undefined
                . uncons
            )
          . splitOn ","
      )
    . (\(x : y : _) -> (x, y))
    . lines
  where
    both :: (a -> b) -> (a, a) -> (b, b)
    both = join bimap
    dired :: Char -> (Integer, Integer)
    dired = \case
      'U' -> (0, 1)
      'D' -> (0, -1)
      'L' -> (-1, 0)
      'R' -> (1, 0)
      els -> error $ "Unrecognized direction: " <> [els]
