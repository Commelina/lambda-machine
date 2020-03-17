{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Machine
-- Copyright   :  (c) Commelina 2020
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  maosics@gmail.com
-- Stability   :  unstable
--
-- A VERY simple 32-bit MIPS CPU with 8 basic instructions
-- (add, sub, and, or, lw, sw, beq and j).
-- And it does not have a specialized RAM (instead we use a vector to
-- simulate it). But if you want, you can implement them and extend
-- its instructions without much effort.
-----------------------------------------------------------------------------

module Machine
       (
         module Machine.Assist
       , module Machine.Debug
       , module Machine.Pretty
       , module Machine.Process
       , module Machine.Types

       , hardware
       , initSysState
       , device
       , topEntity
       ) where

import Clash.Prelude hiding ((<>), empty, tail, (++), cycle)

import Machine.Assist
import Machine.Types
import Machine.Pretty
import Machine.Process
import Machine.Debug

------------------------------------------------------------------------
-- VI. Build a physical machine!
-- So how to do it?
-- You can remember the Merly model: OldState -> Input ->
-- (NewState, Output), and you know something called register which
-- can store a state, then get a input and get a new one, and ...
-- All of them means that we just need to write a function ::
-- s -> i -> (s, o), as a Haskeller, I think you are very familiar
-- with it :)

-- | The state machine model of our simple system.
hardware ::
    SysState ->
    Unsigned 5 ->
    (SysState, Unsigned 32)
hardware s i = (s', getOutput i s')
  where
    s' = cycle s

-- | We have not implemented file I/O, so just hard code the initial
-- state ('registers', 'IRAM', 'DRAM' and 'TempRegs').
initSysState :: SysState
initSysState = SysState initIRAM initDRAM initRegs initTempRegs
  where
    initRegs = Registers
        { cRegs = 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :>
                  0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :>
                  0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :>
                  0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> 0 :> Nil
        , pc    = 0
        }

    initTempRegs = TempRegs
        { _ifid    = IFID  0xfc000000 0
        , _idex  = IDEX  0 0 0 0 0 0 0 0xfc000000 (False,0)
        , _exmem = EXMEM 0 0 0 0 0 NoType
        , _memwb = MEMWB 0 0       NoType
        }
    initIRAM =
        let a     = 0x01094020 :: Unsigned 32 -- add $t0, $t0, $t1
            r     = 0x00004020 :: Unsigned 32 -- add $t0, $zero, $zero
            j     = 0x08000000 :: Unsigned 32 -- j  0x0
            lwt_0 = 0x8c080000 :: Unsigned 32 -- lw $t0, 0($zero)
            j_2   = 0x08000002 :: Unsigned 32 -- j  0x2
            lwt_2 = 0x8e0a0000 :: Unsigned 32 -- lw $t2, 0($s0)
            lwt_3 = 0x8e0b0000 :: Unsigned 32 -- lw $t3, 0($s0)
            lwt_4 = 0x8e0c0000 :: Unsigned 32 -- lw $t4, 0($s0)
            lwt_5 = 0x8e0d0000 :: Unsigned 32 -- lw $t5, 0($s0)
            lwt_6 = 0x8e0e0000 :: Unsigned 32 -- lw $t6, 0($s0)
            lwt_7 = 0x8e0f0000 :: Unsigned 32 -- lw $t7, 0($s0)
        in RAM (lwt_0  :> a :> a :> a :> a :> a :> a :> a :>
                 a     :> a :> a :> a :> a :> a :> a :> a :>
                 a     :> a :> a :> a :> a :> a :> a :> a :>
                 a     :> a :> a :> a :> a :> a :> a :> j :>
                 lwt_0 :> a :> a :> a :> a :> a :> a :> a :>
                 a     :> a :> a :> a :> a :> a :> a :> a :>
                 a     :> a :> a :> a :> a :> a :> a :> a :>
                 a     :> a :> a :> a :> a :> a :> a :> j :> Nil)

    initDRAM =
        let a = 0x00000000 :: Unsigned 32 -- const 0
            b = 0x00001145 :: Unsigned 32 -- const 0x1145
        in RAM (b :> a :> a :> a :> a :> a :> a :> a :>
                a :> a :> a :> a :> a :> a :> a :> a :>
                a :> a :> a :> a :> a :> a :> a :> a :>
                a :> a :> a :> a :> a :> a :> a :> a :>
                a :> a :> a :> a :> a :> a :> a :> a :>
                a :> a :> a :> a :> a :> a :> a :> a :>
                a :> a :> a :> a :> a :> a :> a :> a :>
                a :> a :> a :> a :> a :> a :> a :> a :> Nil)

-- | Clash can generate a mealy model for us.
device
  :: (KnownDomain dom,
      HiddenClockResetEnable dom) =>
     Signal dom (Unsigned 5) ->
     Signal dom (Unsigned 32)
device = mealy hardware initSysState


-- | Top Entity! Here 'exposeClockResetEnable' means that we get input
-- clk, rst and en in compiled verilog code.
{-# ANN topEntity
  (Synthesize
    { t_name   = "SimplePipeline"
    , t_inputs = [PortName "clk", PortName "rst", PortName "en", PortName "Input"]
    , t_output = PortName "Output"
    })#-}
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Unsigned 5)
  -> Signal System (Unsigned 32)
topEntity = exposeClockResetEnable device
