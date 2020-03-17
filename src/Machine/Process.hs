{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Machine.Process
-- Copyright   :  (c) Commelina 2020
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  maosics@gmail.com
-- Stability   :  unstable
--
-- IV. Main part that describes how to get a new state from the old one.
-----------------------------------------------------------------------------

module Machine.Process
       (
         checkStall
       , stall
       , wbProcess
       , memProcess
       , exProcess
       , idProcess
       , ifProcess
       ) where

import Clash.Prelude hiding ((<>), empty, tail, (++), cycle)
import Control.Lens hiding ((:>))
import Data.Maybe (isJust, fromJust)
import Machine.Types
import Machine.Assist


-- Stall
-- | Check whether a stall should be inserted.
checkStall :: SysState -> Bool
checkStall (SysState _ _ _ (TempRegs ifid idex exmem memwb)) =
    opType == RMType && (loadTo == fromS || loadTo == fromT)
  where
    inst    =            ifid ^. ifidInst
    loadTo  =            idex ^. idexDNO
    opType  = decodeOp $ idex ^. idexInst
    fromS   = unpack   $ slice d25 d21 inst
    fromT   = unpack   $ slice d20 d16 inst

-- | Execute a stall cycle.
stall :: SysState -> SysState
stall s = tRegs %~ (\t -> (idex.idexInst) .~ 0xfc000000 $ t) $
          (exProcess (fwd1,fwd2) . memProcess . wbProcess $ s)
  where
    fwd1 = checkForward1 s
    fwd2 = checkForward2 s


-- | The 5th (writing back) stage.
wbProcess :: SysState -> SysState
wbProcess (SysState iram dram regs tempRegs) =
    (SysState iram dram regs' tempRegs)
  where
    regs'
      | tempRegs^.(memwb.memwbInstType) `elem` [RType,RMType] =
          writeRegs regs
          (decodeReg . pack $ tempRegs^.(memwb.memwbDNO))
          (tempRegs^.(memwb.memwbRMData))
      | otherwise = regs

-- | The 4th (memory access) stage.
memProcess :: SysState -> SysState
memProcess (SysState iram dram regs tempRegs) =
    (SysState iram dram' regs' tempRegs')
  where
    oldIfid  = tempRegs^.ifid
    oldIdex  = tempRegs^.idex
    oldExmem = tempRegs^.exmem

    dram' = case oldExmem^.exmemInstType of
              WMType -> writeRAM dram
                        (oldExmem^.exmemALU)
                        (oldExmem^.exmemWMData)
              _      -> dram

    regs' = case oldExmem^.exmemInstType of
              NoType -> regs
              _      -> regs {
                    pc = if oldExmem^.exmemBrch == 1
                         then oldExmem^.exmemJAddr
                         else pc regs
                    }

    memwb' = MEMWB { _memwbRMData = case oldExmem^.exmemInstType of
               RMType -> readRAM dram (oldExmem^.exmemALU)
               RType  -> oldExmem^.exmemALU
               _      -> tempRegs^.(memwb.memwbRMData)
                   , _memwbDNO = oldExmem^.exmemDNO
                   , _memwbInstType   = oldExmem^.exmemInstType
                   }
        -- Predict not taken
    ifid' = if oldExmem^.exmemBrch == 1
            then ifidInst .~ 0xfc000000 $ oldIfid
            else oldIfid

    idex' = if oldExmem^.exmemBrch == 1
            then  idexInst .~ 0xfc000000 $ oldIdex
            else oldIdex
        -- end
    tempRegs'  = tempRegs {
      _ifid    = ifid'
      , _idex  = idex'
      , _memwb = memwb'
      }


-- | The 3rd (execution) stage.
exProcess ::
    (Maybe ForwardTarget, Maybe ForwardTarget)
    -> SysState
    -> SysState
exProcess (fwd1,fwd2) (SysState iram dram regs tempRegs) =
    (SysState iram dram regs tempRegs')
  where
    oldOpType = decodeOp $ oldIdex^.idexInst
    exmem' = EXMEM {
          _exmemJAddr      = case oldOpType of
              JType      -> unpack $ (0 :: BitVector 6) ++#
                            slice d25 d0 (pack $ oldIdex^.idexInst)
              BranchType -> oldIdex^.idexNewpc + oldIdex^.idexImm
              _          -> 0

          , _exmemWMData   = oldIdex^.idexB
          , _exmemInstType = oldOpType
          , _exmemDNO      = oldIdex^.idexDNO
          , _exmemBrch     = case oldOpType of
              JType      -> 1
              BranchType -> if oldIdex^.idexSNO == oldIdex^.idexTNO
                            then 1
                            else 0
              _          -> 0

          , _exmemALU      = case oldOpType of
              -- modified because of forwarding
              RType  -> (decodeFunct $ oldIdex^.idexInst)
                        realRegAData realRegBData
              RMType -> oldIdex^.idexImm + realRegAData
              WMType -> oldIdex^.idexImm + realRegAData
              -- end
              _      -> 0
          }
    tempRegs' = exmem .~ exmem' $ tempRegs
    oldIdex   = tempRegs^.idex
    oldMemwb  = tempRegs^.memwb
    -- forwarding
    realRegAData
      | isJust fwd1 &&
        ((fromJust fwd1 == FWDA) || (fromJust fwd1 == FWDAB))
            = oldMemwb^.memwbRMData
      | isJust fwd2 &&
        ((fromJust fwd2 == FWDA) || (fromJust fwd2 == FWDAB))
            = oldMemwb^.memwbRMData
      | otherwise = oldIdex^.idexA

    realRegBData
      | isJust fwd1 &&
        ((fromJust fwd1 == FWDB) || (fromJust fwd1 == FWDAB))
            = oldMemwb^.memwbRMData
      | isJust fwd1 &&
        ((fromJust fwd1 == FWDB) || (fromJust fwd1 == FWDAB))
            = oldMemwb^.memwbRMData
      | otherwise = oldIdex^.idexB

-- | The 2nd (instruction decode) stage.
idProcess :: SysState -> SysState
idProcess (SysState iram dram regs tempRegs) =
    (SysState iram dram regs tempRegs')
  where
    oldInst = tempRegs^.ifid^.ifidInst
    idex' = IDEX {
      _idexNewpc   = tempRegs^.ifid^.ifidNewpc
      , _idexA     = readRegs regs (decodeRegA oldInst)
      , _idexB     = readRegs regs (decodeRegB oldInst)
      , _idexImm   = signExt oldInst
      , _idexSNO   = unpack $ slice d25 d21 (pack oldInst)
      , _idexTNO   = unpack $ slice d20 d16 (pack oldInst)
      , _idexDNO   = case decodeOp oldInst of
          RType  -> unpack $ slice d15 d11 (pack oldInst)
          RMType -> unpack $ slice d20 d16 (pack oldInst)
          _      -> 0
      , _idexInst  = oldInst -- Nop included
      , _idexStall = (False,0)
      }
    tempRegs' = idex .~ idex' $ tempRegs


-- | The 1st (instruction fetch) stage.
ifProcess :: SysState -> SysState
ifProcess (SysState iram dram regs tempRegs) =
    (SysState iram dram regs' tempRegs')
  where
    newPC = increment $ readRegs regs PC
    regs' = regs { pc = newPC }
    ifid' = IFID {
      _ifidInst    = readRAM iram (readRegs regs PC)
      , _ifidNewpc = newPC
      }
    tempRegs' = ifid .~ ifid' $ tempRegs
