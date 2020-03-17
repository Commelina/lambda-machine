{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE TemplateHaskell   #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Machine.Types
-- Copyright   :  (c) Commelina 2020
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  maosics@gmail.com
-- Stability   :  unstable
--
-- I. Types that describe a Simple CPU
-- Clash requires some of these types to be instances of Generic and
-- NFDataX, which can be auto-derived.
-----------------------------------------------------------------------------

module Machine.Types where

import Clash.Prelude hiding ((<>), empty, tail, (++), cycle)
import Control.Lens hiding ((:>))

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

-- | Registers. It stores specific data.
data Registers = Registers
    { cRegs :: Vec 32 (Unsigned 32)  -- ^ Common-use registers ('R0' ~ 'R31')
    , pc    :: Unsigned 32           -- ^ Program counter
    } deriving (Generic, NFDataX)

-- | RAM. It is just a 64-word vector.
newtype RAM = RAM
    { unRAM :: Vec 64 (Unsigned 32)
    } deriving (Generic, NFDataX)

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

-- | Intermediate registers between 4th (memory access) and
-- 5th (write back) stage.
data MEMWB = MEMWB
    { _memwbRMData   :: Unsigned 32  -- ^ Data to write memory
    , _memwbDNO      :: Unsigned 5   -- ^ Number of RD register
    , _memwbInstType :: OpType       -- Type of stored instruction
    } deriving (Generic, NFDataX)
makeLenses ''MEMWB

-- | Intermediate registers of all stages.
data TempRegs = TempRegs
    { _ifid  :: IFID
    , _idex  :: IDEX
    , _exmem :: EXMEM
    , _memwb :: MEMWB
  } deriving (Generic, NFDataX)
makeLenses ''TempRegs

-- | All the current information of our system, including CPU and RAM.
data SysState = SysState
    { _iram  :: IRAM
    , _dram  :: DRAM
    , _regs  :: Registers
    , _tRegs :: TempRegs
    } deriving (Generic, NFDataX)
makeLenses ''SysState
