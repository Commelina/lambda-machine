{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Machine.Assist
-- Copyright   :  (c) Commelina 2020
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  maosics@gmail.com
-- Stability   :  unstable
--
-- III. Assistant types and functions.
-----------------------------------------------------------------------------

module Machine.Assist
       (
         ForwardTarget(..)

       , cutSegs
       , readRegs
       , writeRegs
       , decodeReg
       , decodeOp
       , decodeFunct
       , decodeRegA
       , decodeRegB
       , signExt
       , applyN
       , getOutput
       , readRAM
       , writeRAM
       , increment
       , isWRType
       , checkForward1
       , checkForward2
       ) where

import Clash.Prelude hiding ((<>), empty, tail, (++), cycle)
import Clash.Sized.Internal.Unsigned (and#, or#)
import Control.Lens hiding ((:>))
import qualified Data.List as L hiding ((!!), cycle)

import Machine.Types

-- | Cut a long list to many fix-length segments.
cutSegs :: [a] -> Int -> [[a]]
cutSegs [] _ = []
cutSegs xs n
    | L.length xs < n = [xs]
    | otherwise       = h : cutSegs t n
        where
          (h,t) = L.splitAt n xs


-- | Read data from specific register.
readRegs :: Registers -> Register -> Unsigned 32
readRegs (Registers cRegs pc) = \case
    PC -> pc
    r  -> cRegs !! regNO
      where
        regNO = read (L.tail $ show r) :: Int

-- | Get register number from a binary sequence.
decodeReg :: BitVector 5 -> Register
decodeReg b = read ("R" L.++ show regNO)
  where
    regNO = unpack b :: Unsigned 5


-- | Run a function N times.
applyN :: Int -> (a -> a) -> a -> a
applyN = (L.foldr (.) id.) . L.replicate


-- | Get operation type from binary instructions.
-- For example, 0b000000... will be converted to 'RType'
decodeOp :: Unsigned 32 -> OpType
decodeOp x = case opPart of
    0  -> RType
    2  -> JType
    4  -> BranchType
    35 -> RMType
    43 -> WMType
    63 -> NoType
    _  -> error "Unimplemented instruction (Op error)"
  where
    opPart :: BitVector 6
    opPart = slice d31 d26 (pack x)

-- | Get function part from binary instructions.
-- For example,  0b...100000 will be converted to '(+)'
decodeFunct ::
    Unsigned 32 ->
    Unsigned 32 -> Unsigned 32 -> Unsigned 32
decodeFunct x = case slice d5 d0 (pack x) of
    32 -> (+)
    34 -> (-)
    36 -> and#
    37 -> or#
    _ -> (+)

-- | Get number of the register connected to ALU's port A.
decodeRegA :: Unsigned 32 -> Register
decodeRegA n = decodeReg $ slice d25 d21 (pack n)

-- | Get number of the register connected to ALU's port B.
decodeRegB :: Unsigned 32 -> Register
decodeRegB n = decodeReg $ slice d20 d16 (pack n)

-- | Sign extend a integer to 32 bit.
signExt :: Unsigned 32 -> Unsigned 32
signExt x = unpack $ higherHalf ++# lowerHalf
  where
    lowerHalf = slice d15 d0 (pack x)
    higherHalf :: BitVector 16
    higherHalf = case msb lowerHalf of
        0 -> 0
        1 -> 65535

-- | Get what a register contains from a state.
-- The input data has 5-bit width because we have 32 registers.
-- This is interesting. Because SysState can describe everything of
-- our system (CPU and RAM), we can just get anything we want (of the
-- system), such as what a register contains.
-- To get the simplest implementation, we just take the lower 8 bits
-- of certain register.
getOutput :: Unsigned 5 -> SysState -> Unsigned 32
getOutput n (SysState _ _ regs _) = readRegs regs reg
  where
    reg = decodeReg $ pack n

-- | Write data to specific register.
writeRegs :: Registers -> Register -> Unsigned 32 -> Registers
writeRegs regs PC x = regs { pc = x }
writeRegs regs  r x = regs { cRegs = replace regNO x (cRegs regs) }
  where
    regNO = read (L.tail $ show r) :: Int

-- | Read data from specific RAM address.
readRAM :: RAM -> Ptr -> Unsigned 32
readRAM (RAM contents) addr = contents !! addr

-- | Write data to specific RAM address.
writeRAM :: RAM -> Ptr -> Unsigned 32 -> RAM
writeRAM (RAM contents) addr x = RAM (replace addr x contents)

-- | Move 'PC' to next instruction. Because we use word address instead of byte
-- address, we take '(+) 1' instead of '(+) 4' here.
increment :: Ptr -> Ptr
increment = (+) 1


-- | Decide if a instruction writes registers.
isWRType :: OpType -> Bool
isWRType RType  = True
isWRType RMType = True
isWRType _      = False


---------------------------------------------------------------------
-- Forwarding

-- | Forwarding target, it can be any of ALU's input port or both.
data ForwardTarget
    = FWDA   -- ^ Only forward to port A
    | FWDB   -- ^ Only forward to port B
    | FWDAB  -- ^ Forward to both port A and B
    deriving (Show, Eq)

-- | Check the forwarding condition 1. Please refer to related books.
checkForward1 :: SysState -> Maybe ForwardTarget
checkForward1 (SysState _ _ _ (TempRegs ifid idex exmem memwb))
  | isW     == False   = Nothing
  | eqS     && not eqT = Just FWDA
  | eqS     && eqT     = Just FWDAB
  | not eqS && eqT     = Just FWDB
  | otherwise          = Nothing
  where
    isW    = isWRType $ exmem^.exmemInstType
    typeST = decodeOp $ idex^.idexInst
    eqS    = typeST /= NoType && exmem^.exmemDNO == idex^.idexSNO
    eqT    = typeST /= NoType && exmem^.exmemDNO == idex^.idexTNO

-- | Check the forwarding condition 2. Please refer to related books.
checkForward2 :: SysState -> Maybe ForwardTarget
checkForward2 (SysState _ _ _ (TempRegs ifid idex exmem memwb))
  | isW                == False                    = Nothing
  | eqS     && not eqT && not eqSExM               = Just FWDA
  | not eqS && eqT     && not eqTExM               = Just FWDB
  | eqS     && eqT     && not eqSExM && not eqTExM = Just FWDAB
  | otherwise                                      = Nothing
  where
    isW       = isWRType $ memwb^.memwbInstType
    typeST    = decodeOp $ idex^.idexInst
    typeSTExM = exmem^.exmemInstType
    eqS       = typeST    /= NoType && memwb^.memwbDNO == idex^.idexSNO
    eqT       = typeST    /= NoType && memwb^.memwbDNO == idex^.idexTNO
    eqSExM    = typeSTExM /= NoType && exmem^.exmemDNO == idex^.idexSNO
    eqTExM    = typeSTExM /= NoType && exmem^.exmemDNO == idex^.idexTNO
