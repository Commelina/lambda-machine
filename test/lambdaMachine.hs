{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

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
         Registers(..)
       , IRAM(..)
       , DRAM(..)
       , IFID(..)
       , IDEX(..)
       , EXMEM(..)
       , MEMWB(..)
       , TempRegs(..)
       , SysState(..)

       , checkStall
       , stall
       , plainCycle
       , cycle
       , runCycle
       , hardware
       , initSysState
       , topEntity
       ) where

import Clash.Prelude hiding ((<>), empty, tail, (++), cycle)
import Clash.Signal
import Clash.Sized.Internal.Unsigned (and#, or#)
import Control.Lens hiding ((:>))
import Data.Maybe (isJust, fromJust)
import GHC.Classes
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass

import qualified Data.List as L hiding ((!!), cycle)


instance Pretty (Unsigned n) where
    pPrint = text . show

-- | Cut a long list to many fix-length segments.
cutSegs :: [a] -> Int -> [[a]]
cutSegs [] _ = []
cutSegs xs n
    | L.length xs < n = [xs]
    | otherwise       = h : cutSegs t n
        where
          (h,t) = L.splitAt n xs

------------------------------------------------------------------------
-- I. Types that describe a Simple CPU
-- Clash requires some of these types to be instances of Generic and
-- NFDataX, which can be auto-derived.

-- | Pointer. It is just a 32 bit unsigned integer and only used by PC.
type Ptr = Unsigned 32

-- | Register. It is used for type-safety and does not store data.
data Register
    = R0  | R1  | R2  | R3  | R4  | R5
    | R6  | R7  | R8  | R9  | R10 | R11
    | R12 | R13 | R14 | R15 | R16 | R17
    | R18 | R19 | R20 | R21 | R22 | R23
    | R24 | R25 | R26 | R27 | R28 | R29
    | R30 | R31 | PC
    deriving (Show, Read)

-- | Instructions. More of them can be added later.
data Instruction
    = Add Register Register Register
    | Sub Register Register Register
    | And Register Register Register
    | Or  Register Register Register
    | Lw  Register Register (Unsigned 32)
    | Sw  Register Register (Unsigned 32)
    | Beq Register Register (Unsigned 32)
    | J (Unsigned 32)
    | Nop  -- ^ Empty instruction, does nothing.
    deriving (Show)

-- | Type of instructions.
data OpType
    = RType       -- ^ "R" type such as arithmetical and bit operations
    | BranchType  -- ^ Branch instructions such as BEQ
    | JType       -- ^ Jump type such as J
    | RMType      -- ^ Instructions which read memory
    | WMType      -- ^ Instructions which write memory
    | NoType      -- ^ Empty instruction, does nothing
    deriving (Show, Eq, Generic, NFDataX)

instance Pretty OpType where
    pPrint t = text (show t)


-- | Registers. It stores specific data.
data Registers = Registers
    { cRegs :: Vec 32 (Unsigned 32)  -- ^ Common-use registers ('R0' ~ 'R31')
    , pc    :: Unsigned 32           -- ^ Program counter
    } deriving (Generic, NFDataX)

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

instance Pretty Registers where
    pPrint r = text "Common-use regs: " $$
               nest 4 (fsep cDocs)      $$
               text "PC: " <> pcDoc
      where
        cDocs = pPrint <$> cutSegs (toList $ cRegs r) 8
        pcDoc = pPrint (pc r)
instance Show Registers where
    show = render . pPrint

-- | RAM. It is just a 64-word vector.
newtype RAM = RAM
    { unRAM :: Vec 64 (Unsigned 32)
    } deriving (Generic, NFDataX)

instance Pretty RAM where
    pPrint r = nest 4 (fsep docs)
      where
        docs = pPrint <$> cutSegs (toList $ unRAM r) 8
instance Show RAM where
  show = render . pPrint

-- | Instruction RAM.
type IRAM = RAM

-- | Data RAM.
type DRAM = RAM

-- | Intermediate registers between 1st (instruction fetch) and
-- 2nd (instruction decode) stage.
data IFID = IFID
    { _ifidInst  :: Unsigned 32  -- ^ Stored instruction
    , _ifidNewpc :: Unsigned 32  -- ^ New pc after fetching instruction
    } deriving (Generic, NFDataX)
makeLenses ''IFID

instance Pretty IFID where
    pPrint ifid = nest 4 $ vcat
      [ text "ifidInst: "  <> pPrint (ifid ^. ifidInst)
      , text "ifidNewpc: " <> pPrint (ifid ^. ifidNewpc)
      ]

-- | Intermediate registers between 2nd (instruction decode) and
-- 3rd (execution) stage.
data IDEX = IDEX
    { _idexNewpc :: Unsigned 32        -- ^ New pc passed from 'IFID'
    , _idexA     :: Unsigned 32        -- ^ Data fetched from register A
    , _idexB     :: Unsigned 32        -- ^ Data fetched from register B
    , _idexImm   :: Unsigned 32        -- ^ Extended immediate
    , _idexSNO   :: Unsigned 5         -- ^ Number of RS register
    , _idexTNO   :: Unsigned 5         -- ^ Number of RT register
    , _idexDNO   :: Unsigned 5         -- ^ Number of RD register
    , _idexInst  :: Unsigned 32        -- ^ Stored instruction
    , _idexStall :: (Bool,Unsigned 2)  -- ^ Stall status of this time
    } deriving (Generic, NFDataX)
makeLenses ''IDEX

instance Pretty IDEX where
    pPrint idex = nest 4 $ vcat
      [ text "idexNewpc: " <>  pPrint (idex ^. idexNewpc)
      , text "idexA: "     <>  pPrint (idex ^. idexA)
      , text "idexB: "     <>  pPrint (idex ^. idexB)
      , text "idexImm: "   <>  pPrint (idex ^. idexImm)
      , text "idexSNO: "   <>  pPrint (idex ^. idexSNO)
      , text "idexTNO: "   <>  pPrint (idex ^. idexTNO)
      , text "idexDNO: "   <>  pPrint (idex ^. idexDNO)
      , text "idexInst: "  <>  pPrint (idex ^. idexInst)
      , text "idexStall: " <>  pPrint (idex ^. idexStall)
      ]

-- | Intermediate registers between 3rd (execution) and
-- 4th (memory access) stage.
data EXMEM = EXMEM
    { _exmemJAddr    :: Unsigned 32  -- ^ Jump target address
    , _exmemALU      :: Unsigned 32  -- ^ Result of ALU unit
    , _exmemWMData   :: Unsigned 32  -- ^ Data to write memory, equals to regB
    , _exmemBrch     :: Unsigned 1   -- ^ Branch flag
    , _exmemDNO      :: Unsigned 5   -- ^ Number of RD register, same in IDEX, EXMEM and MEMWB stages
    , _exmemInstType :: OpType       -- ^ Type of stored instruction
    } deriving (Generic, NFDataX)
makeLenses ''EXMEM

instance Pretty EXMEM where
    pPrint exmem = nest 4 $ vcat
      [ text "exmemJAddr: "    <> pPrint (exmem ^. exmemJAddr)
      , text "exmemALU: "      <> pPrint (exmem ^. exmemALU)
      , text "exmemWMData: "   <> pPrint (exmem ^. exmemWMData)
      , text "exmemBrch: "     <> pPrint (exmem ^. exmemBrch)
      , text "exmemDNO: "      <> pPrint (exmem ^. exmemDNO)
      , text "exmemInstType: " <> pPrint (exmem ^. exmemInstType)
      ]

-- | Intermediate registers between 4th (memory access) and
-- 5th (write back) stage.
data MEMWB = MEMWB
    { _memwbRMData   :: Unsigned 32  -- ^ Data to write memory
    , _memwbDNO      :: Unsigned 5   -- ^ Number of RD register
    , _memwbInstType :: OpType       -- Type of stored instruction
    } deriving (Generic, NFDataX)
makeLenses ''MEMWB

instance Pretty MEMWB where
    pPrint memwb = nest 4 $ vcat
      [ text "memwbRMData: "   <> pPrint (memwb ^. memwbRMData)
      , text "memwbDNO: "      <> pPrint (memwb ^. memwbDNO)
      , text "memwbInstType: " <> pPrint (memwb ^. memwbInstType)
      ]


-- | Intermediate registers of all stages.
data TempRegs = TempRegs
    { _ifid  :: IFID
    , _idex  :: IDEX
    , _exmem :: EXMEM
    , _memwb :: MEMWB
  } deriving (Generic, NFDataX)
makeLenses ''TempRegs

instance Pretty TempRegs where
    pPrint tempRegs = vcat
      [ text "IFID: "
      , pPrint $ tempRegs ^. ifid
      , text "IDEX: "
      , pPrint $ tempRegs ^. idex
      , text "EXMEM: "
      , pPrint $ tempRegs ^. exmem
      , text "MEMWB: "
      , pPrint $ tempRegs ^. memwb
      ]
instance Show TempRegs where
    show = render . pPrint

-- | All the current information of our system, including CPU and RAM.
data SysState = SysState
    { _iram  :: IRAM
    , _dram  :: DRAM
    , _regs  :: Registers
    , _tRegs :: TempRegs
    } deriving (Generic, NFDataX)
makeLenses ''SysState

instance Pretty SysState where
    pPrint (SysState i d r t) =
        text "IRAM: " $$ pPrint i $$
        text "DRAM: " $$ pPrint d $$
        pPrint r $$
        pPrint t
instance Show SysState where
    show = render . pPrint




------------------------------------------------------------------------
-- II. Some assistant functions. They are really clear.
-- Not all of assistant functions are listed here, you can find others in [assist] blocks of cycle function.

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
    _ -> (+)    -- ^ We take '(+)' as default result

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


------------------------------------------------------------------------
-- III. Main part that describes how to get a new state from the old one.

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

-- | Plain cycle without any stall inserted. It can result in incorrect
-- result because of data race.
plainCycle :: SysState -> SysState
plainCycle s =
  ifProcess . idProcess . exProcess (fwd1,fwd2) . memProcess . wbProcess $ s
  where
    fwd1 = checkForward1 s
    fwd2 = checkForward2 s

-- | Normal cycle with necessary stalls inserted.
cycle :: SysState -> SysState
cycle s
  | checkStall s = case s ^. (tRegs.idex.idexStall) of
      (False,0) -> (tRegs.idex.idexStall._1) %~ not $ stall s
      _         -> error "Stall error"
  | otherwise = plainCycle s

-- | Run many cycles from a initial state and return the final state.
runCycle :: SysState -> Int -> SysState
runCycle s n = applyN n Machine.cycle s

---------------------------------------------------------------------
-- Forwarding Part

-- | Decide if a instruction writes registers.
isWRType :: OpType -> Bool
isWRType RType  = True
isWRType RMType = True
isWRType _      = False

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


---------------------------------------------------------------------
-- Each detailed stage.

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
      , _idexImm   = Machine.signExt oldInst
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

------------------------------------------------------------------------
-- IV. Build a physical machine!
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
    s' = Machine.cycle s

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
