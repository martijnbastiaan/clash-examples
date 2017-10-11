module Lib where

import qualified Prelude
import qualified Data.List

import Control.Concurrent (threadDelay)
import Clash.Intel.ClockGen
import Clash.Explicit.Prelude hiding (empty)
import Clash.Signal (Signal)
import Data.Int (Int32)
import Data.Maybe
import Debug.Trace
import System.IO

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

waitFor = 50000000 `quot` 1

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

nothing = (0, 0, 0, 0, 0, 1, 1)

tester :: State
       -> ( Bit            -- Random bit from LSFR
          , BitVector 16   -- readdata
          , Bool           -- readdatavalid
          , Bool           -- waitrequest
          )
       -> (State, (
            BitVector 10 -- LEDS
          , BitVector 25 -- addr
          , BitVector 2  -- byteenable_n
          , BitVector 1  -- chipselect
          , BitVector 16 -- writedata
          , Bit          -- read_n
          , Bit          -- write_n
       ))
-- Pause if memory controller is not ready
tester s (_, _, _, True)  = (s, nothing)

-- Wait for user specified time, then move on to write (and update random value)
tester (DELAY, addr, cntr, random) (bit, readdata, readdatavalid, waitrequest) = (s, o)
  where
   o = (fromIntegral addr, 0, 0, 0, 0, 1, 1) -- Display current address on LEDs
   s = (if cntr >= waitFor then WRITE else DELAY, addr, succ cntr, shiftIn random bit)

-- Write value and immediately move onto READ
tester (WRITE, addr, cntr, random) _  = ( (READ, addr, cntr, random)
                                        , (0, pack addr, minBound, 1, random, 1, 0)
                                        )

-- Read value (wait until memory controller is ready)
tester (READ, addr, cntr, random) _ = ( (WAIT, addr, cntr, random)
                                      , (0, pack addr, minBound, 1, 0, 0, 1)
                                      )

-- Wait for value to arrive
tester s@(WAIT, addr, cntr, random) (_, readdata, False, _) = (s, nothing)
tester s@(WAIT, addr, cntr, random) (_, readdata, True,  _) = ( (next, succ addr, 0, random)
                                                              , nothing
                                                              )
                                                                where
                                                                  next | readdata == random = DELAY
                                                                       | otherwise          = ERROR

-- Error: flash LEDs like crazy
tester (ERROR, _, cntr, random) _ = ((ERROR, 0, cntr', random'), nothing)
  where
    cntr' | cntr >= waitFor = 0
          | otherwise       = succ cntr

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
g clk rst readdata datavalid waitrq = o
  where
    randomBits = lfsrF clk rst 0xAAAA
    i = (randomBits, readdata, tobool <$> datavalid, tobool <$> waitrq)
    o = mealyB clk rst tester (DELAY, 0, 0, 0) i

