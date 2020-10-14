{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Day 2: 1202 Program Alarm
module Y2019.D02 (d02a, d02b) where

import Control.Applicative (liftA2)
import Control.Exception (assert)
import Data.Bifunctor (first)
import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as S
import Data.Text (Text, null, splitOn)
import Data.Text.Read (decimal)
import Numeric.Natural (Natural)
import Prelude hiding (null)

-- | On the way to your
-- <https://en.wikipedia.org/wiki/Gravity_assist gravity assist> around the
-- Moon, your ship computer beeps angrily about a
-- "<https://www.hq.nasa.gov/alsj/a11/a11.landing.html#1023832 1202 program alarm>".
-- On the radio, an Elf is already explaining how to handle the situation:
-- "Don't worry, that's perfectly norma--" The ship computer
-- <https://en.wikipedia.org/wiki/Halt_and_Catch_Fire bursts into flames>.
--
-- You notify the Elves that the computer's
-- <https://en.wikipedia.org/wiki/Magic_smoke magic smoke> seems to have
-- escaped. "That computer ran __Intcode__ programs like the gravity assist
-- program it was working on; surely there are enough spare parts up there to
-- build a new Intcode computer!"
--
-- An Intcode program is a list of integers separated by commas (like
-- @1,0,0,3,99@). To run one, start by looking at the first integer (called
-- position @0@). Here, you will find an __opcode__ - either @1@, @2@, or @99@.
-- The opcode indicates what to do; for example, @99@ means that the program is
-- finished and should immediately halt. Encountering an unknown opcode means
-- that something went wrong.
--
-- Opcode @1@ __adds__ together numbers read from two positions and stores the
-- result in a third position. The three integers __immediately after__ the
-- opcode tell you these three positions - the first two indicate the
-- __positions__ from which you should read the input values, and the third
-- indicates the __position__ at which the output should be stored.
--
-- For example, if your Intcode computer encounters @1,10,20,30@, it should read
-- the values at positions @10@ and @20@, add those values, and then overwrite
-- the value at position @30@ with their sum.
--
-- Opcode @2@ works exactly like opcode @1@, except it __multiplies__ the two
-- inputs instead of adding them. Again, the three integers after the opcode
-- indicate __where__ the inputs and outputs are, not their values.
--
-- Once you're done processing an opcode, __move to the next one__ by stepping
-- forward @4@ positions.
--
-- For example, suppose you have the following program:
--
-- @1,9,10,3,2,3,11,0,99,30,40,50@
--
-- For the purposes of illustration, here is the same program split into
-- multiple lines:
--
-- @
-- 1,9,10,3,
-- 2,3,11,0,
-- 99,
-- 30,40,50
-- @
--
-- The first four integers, @1,9,10,3@, are at positions @0@, @1@, @2@, and @3@.
-- Together, they represent the first opcode (@1@, addition), the positions of
-- the two inputs (@9@ and @10@), and the position of the output (@3@). To
-- handle this opcode, you first need to get the values at the input positions:
-- position @9@ contains @30@, and position @10@ contains @40@. __Add__ these
-- numbers together to get @70@. Then, store this number at the output position;
-- here, the output position (@3@) is __at__ position @3@, so it overwrites
-- itself. Afterward, the program looks like this:
--
-- @
-- 1,9,10,__70__,
-- 2,3,11,0,
-- 99,
-- 30,40,50
-- @
--
-- Step forward 4 positions to reach the next opcode, @2@. This opcode works
-- just like the previous, but it multiplies instead of adding. The inputs are
-- at positions @3@ and @11@; these positions contain @70@ and @50@
-- respectively. Multiplying these produces @3500@; this is stored at position
-- @0@:
--
-- @
-- __3500__,9,10,70,
-- 2,3,11,0,
-- 99,
-- 30,40,50
-- @
--
-- Stepping forward @4@ more positions arrives at opcode @99@, halting the
-- program.
--
-- Here are the initial and final states of a few more small programs:
--
--   -  @1,0,0,0,99@ becomes @__2__,0,0,0,99@ (@1 + 1 = 2@).
--   -  @2,3,0,3,99@ becomes @2,3,0,__6__,99@ (@3 * 2 = 6@).
--   -  @2,4,3,4,99,0@ becomes @2,4,4,5,99,__9801__@ (@99 * 99 = 9801@).
--   -  @1,1,1,4,99,5,6,0,99@ becomes @__30__,1,1,4,__2__,5,6,0,99@.
--
-- Once you have a working computer, the first step is to restore the gravity
-- assist program (your puzzle input) to the "1202 program alarm" state it had
-- just before the last computer caught fire. To do this,
-- __before running the program__, replace position @1@ with the value @12@ and
-- replace position @2@ with the value @2@.
-- __What value is left at position @0@__ after the program halts?
d02a :: Text -> Natural
d02a =
  lookup' 0
    . snd
    . iter
      ( \(!ptr, !lst) ->
          let [arg1, arg2, arg3] =
                map (\x -> fromIntegral $ lookup' (ptr + x) lst) [1, 2, 3]
           in (ptr + 4,) <$> case lookup' ptr lst of
                1 ->
                  Just $ S.update arg3 (lookup' arg1 lst + lookup' arg2 lst) lst
                2 ->
                  Just $ S.update arg3 (lookup' arg1 lst * lookup' arg2 lst) lst
                99 -> Nothing
                els -> error $ "Unrecognized opcode: " <> show els
      )
    . (0,)
    . S.update 2 2
    . S.update 1 12
    . S.fromList
    . map
      ( uncurry (flip (assert . null))
          . either error id
          . decimal
      )
    . splitOn ","
  where
    lookup' :: Int -> S.Seq a -> a
    lookup' = fromMaybe undefined .* S.lookup
    (.*) = (.) . (.)

--- Part Two ---

-- | "Good, the new computer seems to be working correctly! __Keep it nearby__
-- during this mission - you'll probably use it again. Real Intcode computers
-- support many more features than your new one, but we'll let you know what
-- they are as you need them."
--
-- "However, your current priority should be to complete your gravity assist
-- around the Moon. For this mission to succeed, we should settle on some
-- terminology for the parts you've already built."
--
-- Intcode programs are given as a list of integers; these values are used as
-- the initial state of the computer's __memory__. When you run an Intcode
-- program, make sure to start by initializing memory to the program's values. A
-- position in memory is called an __address__ (for example, the first value in
-- memory is at "address 0").
--
-- Opcodes (like @1@, @2@, or @99@) mark the beginning of an __instruction__.
-- The values used immediately after an opcode, if any, are called the
-- instruction's __parameters__. For example, in the instruction @1,2,3,4@, @1@
-- is the opcode; @2@, @3@, and @4@ are the parameters. The instruction @99@
-- contains only an opcode and has no parameters.
--
-- The address of the current instruction is called the __instruction pointer__;
-- it starts at @0@. After an instruction finishes, the instruction pointer
-- increases by __the number of values in the instruction__; until you add more
-- instructions to the computer, this is always @4@ (@1@ opcode + @3@
-- parameters) for the add and multiply instructions. (The halt instruction
-- would increase the instruction pointer by @1@, but it halts the program
-- instead.)
--
-- "With terminology out of the way, we're ready to proceed. To complete the
-- gravity assist, you need to
-- __determine what pair of inputs produces the output @19690720@__."
--
-- The inputs should still be provided to the program by replacing the values at
-- addresses 1 and 2, just like before. In this program, the value placed in
-- address @1@ is called the _noun_, and the value placed in address @2@ is
-- called the __verb__. Each of the two input values will be between @0@ and
-- @99@, inclusive.
--
-- Once the program has halted, its output is available at address @0@, also
-- just like before. Each time you try a pair of inputs, make sure you first
-- __reset the computer's memory to the values in the program__ (your puzzle
-- input) - in other words, don't reuse memory from a previous attempt.
--
-- Find the input __noun__ and __verb__ that cause the program to produce the
-- output @19690720@. __What is @100 * noun + verb@?__ (For example, if
-- @noun=12@ and @verb=2@, the answer would be @1202@.)
d02b :: Text -> Natural
d02b input =
  uncurry (+)
    . first (* 100)
    . fromMaybe undefined
    . find
      ( \(!one, !two) ->
          (== 19690720)
            . lookup' 0
            . snd
            . iter
              ( \(!ptr, !lst) ->
                  let [arg1, arg2, arg3] =
                        map (\x -> fromIntegral $ lookup' (ptr + x) lst) [1, 2, 3]
                   in (ptr + 4,) <$> case lookup' ptr lst of
                        1 ->
                          Just $ S.update arg3 (lookup' arg1 lst + lookup' arg2 lst) lst
                        2 ->
                          Just $ S.update arg3 (lookup' arg1 lst * lookup' arg2 lst) lst
                        99 -> Nothing
                        els -> error $ "Unexpected opcode: " <> show els
              )
            . (0,)
            . S.update 2 two
            . S.update 1 one
            $ sqnc
      )
    $ dup (liftA2 (,)) [0 .. 99]
  where
    sqnc =
      S.fromList
        . map
          ( uncurry (flip (assert . null))
              . either error id
              . decimal
          )
        . splitOn ","
        $ input
    lookup' :: Int -> S.Seq a -> a
    lookup' = fromMaybe undefined .* S.lookup
    (.*) = (.) . (.)
    dup :: (a -> a -> b) -> a -> b
    dup f x = f x x

iter :: forall a. (a -> Maybe a) -> a -> a
iter f = go
  where
    go :: a -> a
    go seed = maybe seed go $ f seed
