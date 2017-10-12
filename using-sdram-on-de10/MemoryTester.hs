{-|
Copyright   : (C) 2017, QBayLogic
License     : BSD2 (see the file LICENSE)
Module      : MemoryTester
Description : SDRAM health tester, fitting blogpost "Using SDRAM on the DE10 using Clash"
Author      : Martijn Bastiaan <martijn@qbaylogic.com>
Maintainer  : Martijn Bastiaan <martijn@qbaylogic.com>
Stability   : experimental
Portability : POSIX

Simple state machine which cycles between:

  - Showing intermediate results to user
  - Writing random number to memory address
  - Reading from same memory address and checking for consistency

An error state is triggered whenever a consistency check fails.
-}
module MemoryTester where

import Clash.Explicit.Prelude
import Clash.Signal (Signal)
import Data.Int (Int32,Int64)
import Data.Maybe

data Mode = DELAY -- Take some time to display some code to the user
          | ERROR -- Mode to communicate we've encountered an error
          | WRITE -- Write random number to current memory address
          | READ  -- Read random number from current memory address
          | WAIT  -- Wait for random number to appear

type State = (
               Mode
             , Index (2^25)
             , Index (2^32)
             , BitVector 16 -- Random number (updated during DELAY)
             )

-- We would like to display our memory test as a progress bar on the
-- LEDs. This function converts a memory address to a number of lit
-- LEDs.
progressBar :: Index (2^25)
            -- ^ Number
            -> BitVector 10
            -- ^ Progress expressed as LEDs
progressBar n = shiftL (complement zeroBits) (10 - nLeds)
  where
    step = maxBound `quot` 11
    nLeds | n < 1*step  = 0
          | n < 2*step  = 1
          | n < 3*step  = 2
          | n < 4*step  = 3
          | n < 5*step  = 4
          | n < 6*step  = 5
          | n < 7*step  = 6
          | n < 8*step  = 7
          | n < 9*step  = 8
          | n < 10*step = 9
          | otherwise   = 10


lfsrF' :: BitVector 16 -> BitVector 16
lfsrF' s = feedback ++# slice d15 d1 s
  where
    feedback = s!5 `xor` s!3 `xor` s!2 `xor` s!0

lfsrF :: Clock System Source
      -> Reset System Asynchronous
      -> BitVector 16
      -> Signal System Bit
lfsrF clk rst seed = msb <$> r
  where r = register clk rst seed (lfsrF' <$> r)


shiftIn :: KnownNat n => BitVector n -> Bit -> BitVector n
shiftIn vec bit = (if bit == 0 then clearBit else setBit) (shiftR vec 1) 0

nothing addr = (progressBar addr, 0, 0, 1, 1)

tester :: State
       -> ( Bit            -- Random bit from LSFR
          , BitVector 16   -- readdata
          , Bool           -- readdatavalid
          , Bool           -- waitrequest
          )
       -> (State, (
            BitVector 10 -- LEDS
          , BitVector 25 -- addr
          , BitVector 16 -- writedata
          , Bit          -- read_n
          , Bit          -- write_n
       ))
-- Pause if memory controller is not ready
tester s@(_, addr, _, _) (_, _, _, True)  = (s, nothing addr)

-- Wait for 16 clockticks to update random number, then move on to write
tester (DELAY, addr, cntr, random) (bit, readdata, readdatavalid, waitrequest) = (s, o)
  where
   o = (progressBar addr, 0, 0, 1, 1) -- Display current address on LEDs
   s = (if cntr >= 16 then WRITE else DELAY, addr, succ cntr, shiftIn random bit)

-- Write value and immediately move onto READ
tester (WRITE, addr, cntr, random) _  = ( (READ, addr, cntr, random)
                                        , (0, pack addr, random, 1, 0)
                                        )

-- Read value (wait until memory controller is ready)
tester (READ, addr, cntr, random) _ = ( (WAIT, addr, cntr, random)
                                      , (0, pack addr, 0, 0, 1)
                                      )

-- Wait for value to arrive
tester s@(WAIT, addr, cntr, random) (_, readdata, False, _) = (s, nothing addr)
tester s@(WAIT, addr, cntr, random) (_, readdata, True,  _) = ( (next, succ addr, 0, random)
                                                              , nothing addr
                                                              )
                                                                where
                                                                  next | readdata == random = DELAY
                                                                       | otherwise          = ERROR

-- Error: flash LEDs like crazy
tester (ERROR, addr, cntr, random) _ = ((ERROR, addr, cntr', random'), (resize random, 0, 0, 1, 1))
  where
    cntr' | cntr >= 50000000 = 0
          | otherwise        = succ cntr

    random' | cntr == 0 = complement random
            | otherwise = random


tobool :: Bit -> Bool
tobool 1 = True
tobool 0 = False

{-# ANN g
  (defTop{
    t_name = "sdramh",
    t_inputs = [
                 PortName "CLOCK_50"
               , PortName "RESET"
               , PortName "readdata"
               , PortName "readdatavalid"
               , PortName "waitrequest"
               ],
    t_output = PortField "Output" [
                 PortName "LEDR"
               , PortName "sdramc_s1_addr"
               , PortName "sdramc_s1_byteenable_n"
               , PortName "sdramc_s1_chipselect"
               , PortName "sdramc_s1_writedata"
               , PortName "sdramc_s1_read_n"
               , PortName "sdramc_s1_write_n"
               ]
  })
  #-}
g ::
     Clock System Source            -- CLOCK (50 MHz)
  -> Reset System Asynchronous      -- RESET
  -> Signal System (BitVector 16)   -- readdata
  -> Signal System Bit              -- readdatavalid
  -> Signal System Bit              -- waitrequest
  -> ( Signal System (BitVector 10) -- LEDS
     , Signal System (BitVector 25) -- addr
     , Signal System (BitVector 2)  -- byteenable_n
     , Signal System (BitVector 1)  -- chipselect
     , Signal System (BitVector 16) -- writedata
     , Signal System Bit            -- read_n
     , Signal System Bit            -- write_n
     )
g clk rst readdata datavalid waitrq = (leds, addr, 0, 1, writedata, read_n, write_n)
  where
    randomBits = lfsrF clk rst 0xAAAA
    i = (randomBits, readdata, tobool <$> datavalid, tobool <$> waitrq)
    (leds, addr, writedata, read_n, write_n) = mealyB clk rst tester (DELAY, 0, 0, 0) i

