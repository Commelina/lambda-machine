{-# LANGUAGE BangPatterns #-}

{-
  This is a VERY simple 32-bit MIPS CPU with 8 basic instructions(add, sub, and, or, lw, sw, beq and j).
  And it does not have a specialized RAM (instead we use a vector to simulate it).
  But if you want, you can implement them without much effort :)
-}

module CPU where

import Clash.Prelude
import Clash.Sized.Internal.Unsigned (and#, or#)

-- I. Types that describe a CPU
{-
  Clash requires some of these types to be instances of Generic and NFDataX.
  Generic can be auto-derived and we can give NFDataX the simplest implementation
-}

-- + pointer. It is just a 32 bit number and only used by pc
newtype Ptr = Ptr (Unsigned 32) deriving (Show, Generic)
instance NFDataX Ptr where
  rnfX !_ = ()

getPtrContents :: Ptr -> Unsigned 32
getPtrContents (Ptr x) = x

-- + Register. It is used for type-safety and does not store data
data Register = R0  | R1  | R2  | R3  | R4  | R5
              | R6  | R7  | R8  | R9  | R10 | R11
              | R12 | R13 | R14 | R15 | R16 | R17
              | R18 | R19 | R20 | R21 | R22 | R23
              | R24 | R25 | R26 | R27 | R28 | R29
              | R30 | R31 deriving (Show)

-- + Instruction. More of them can be added later
data Instruction = Add Register Register Register
                 | Sub Register Register Register
                 | And Register Register Register
                 | Or Register Register Register
                 | Lw Register Register (Unsigned 32)
                 | Sw Register Register (Unsigned 32)
                 | Beq Register Register (Unsigned 32)
                 | J (Unsigned 32)
                 deriving (Show)

-- + Type of instructions.
data OpType = RType | BranchType | JumpType | ReadMemType | WriteMemType deriving (Show)

-- + Current stage of CPU.
data CPUActivity = InstFetch | InstDecode | Execute | MemProcess | FinishReadMem deriving (Show, Generic)
instance NFDataX CPUActivity where
  rnfX !_ = ()

-- + Registers. It stores specific data
data Registers = Registers {
  r0  :: Unsigned 32,
  r1  :: Unsigned 32,
  r2  :: Unsigned 32,
  r3  :: Unsigned 32,
  r4  :: Unsigned 32,
  r5  :: Unsigned 32,
  r6  :: Unsigned 32,
  r7  :: Unsigned 32,
  r8  :: Unsigned 32,
  r9  :: Unsigned 32,
  r10 :: Unsigned 32,
  r11 :: Unsigned 32,
  r12 :: Unsigned 32,
  r13 :: Unsigned 32,
  r14 :: Unsigned 32,
  r15 :: Unsigned 32,
  r16 :: Unsigned 32,
  r17 :: Unsigned 32,
  r18 :: Unsigned 32,
  r19 :: Unsigned 32,
  r20 :: Unsigned 32,
  r21 :: Unsigned 32,
  r22 :: Unsigned 32,
  r23 :: Unsigned 32,
  r24 :: Unsigned 32,
  r25 :: Unsigned 32,
  r26 :: Unsigned 32,
  r27 :: Unsigned 32,
  r28 :: Unsigned 32,
  r29 :: Unsigned 32,
  r30 :: Unsigned 32,
  r31 :: Unsigned 32,
  pc :: Ptr
  } deriving (Show, Generic)
instance NFDataX Registers where
  rnfX !_ = ()

-- + All the current information of our CPU.
data CPUState = CPUState CPUActivity Registers deriving (Show, Generic)
instance NFDataX CPUState where
  rnfX !_ = ()

-- + RAM. It is just a 64-word vector
data RAM = RAM (Vec 64 (Unsigned 32)) deriving (Show, Generic)
instance NFDataX RAM where
  rnfX !_ = ()

-- + Intermediate registers.
data TempRegs = TempRegs {
  ir :: Unsigned 32,
  mdr :: Unsigned 32,
  ra :: Unsigned 32,
  rb :: Unsigned 32,
  aluout :: Unsigned 32
  } deriving (Show, Generic)
instance NFDataX TempRegs where
  rnfX !_ = ()

-- + All the current information of our system, including CPU and RAM.
type SystemState = (CPUState, RAM, TempRegs)



-- II. Some assistant functions. They are really clear.
--     Not all of them is as follows, you can find others in [assist] blocks of cycle function.
readRegister :: Registers -> Register -> Unsigned 32
readRegister (Registers r0 r1 r2 r3 r4 r5 r6 r7
              r8 r9 r10 r11 r12 r13 r14 r15 r16
              r17 r18 r19 r20 r21 r22 r23 r24
              r25 r26 r27 r28 r29 r30 r31 _) r = case r of
  R0  -> r0
  R1  -> r1
  R2  -> r2
  R3  -> r3
  R4  -> r4
  R5  -> r5
  R6  -> r6
  R7  -> r7
  R8  -> r8
  R9  -> r9
  R10 -> r10
  R11 -> r11
  R12 -> r12
  R13 -> r13
  R14 -> r14
  R15 -> r15
  R16 -> r16
  R17 -> r17
  R18 -> r18
  R19 -> r19
  R20 -> r20
  R21 -> r21
  R22 -> r22
  R23 -> r23
  R24 -> r24
  R25 -> r25
  R26 -> r26
  R27 -> r27
  R28 -> r28
  R29 -> r29
  R30 -> r30
  R31 -> r31

writeRegister :: Registers -> Register -> Unsigned 32 -> Registers
writeRegister regs r x = case r of
  R0  -> regs { r0  = x }
  R1  -> regs { r1  = x }
  R2  -> regs { r2  = x }
  R3  -> regs { r3  = x }
  R4  -> regs { r4  = x }
  R5  -> regs { r5  = x }
  R6  -> regs { r6  = x }
  R7  -> regs { r7  = x }
  R8  -> regs { r8  = x }
  R9  -> regs { r9  = x }
  R10 -> regs { r10 = x }
  R11 -> regs { r11 = x }
  R12 -> regs { r12 = x }
  R13 -> regs { r13 = x }
  R14 -> regs { r14 = x }
  R15 -> regs { r15 = x }
  R16 -> regs { r16 = x }
  R17 -> regs { r17 = x }
  R18 -> regs { r18 = x }
  R19 -> regs { r19 = x }
  R20 -> regs { r20 = x }
  R21 -> regs { r21 = x }
  R22 -> regs { r22 = x }
  R23 -> regs { r23 = x }
  R24 -> regs { r24 = x }
  R25 -> regs { r25 = x }
  R26 -> regs { r26 = x }
  R27 -> regs { r27 = x }
  R28 -> regs { r28 = x }
  R29 -> regs { r29 = x }
  R30 -> regs { r30 = x }
  R31 -> regs { r31 = x }

readRAM :: RAM -> Ptr -> Unsigned 32
readRAM (RAM contents) (Ptr addr) = contents !! addr

writeRAM :: RAM -> Ptr -> Unsigned 32 -> RAM
writeRAM (RAM contents) (Ptr addr) x = RAM (replace addr x contents)

-- We use word address instead of byte address
increment :: Ptr -> Ptr
increment (Ptr addr) = Ptr (addr + 1)

-- Get register from binary instructions.
-- e.g. 00101 -> R5
decodeReg :: BitVector 5 -> Register
decodeReg x = case x of
  0  -> R0
  1  -> R1
  2  -> R2
  3  -> R3
  4  -> R4
  5  -> R5
  6  -> R6
  7  -> R7
  8  -> R8
  9  -> R9
  10 -> R10
  11 -> R11
  12 -> R12
  13 -> R13
  14 -> R14
  15 -> R15
  16 -> R16
  17 -> R17
  18 -> R18
  19 -> R19
  20 -> R20
  21 -> R21
  22 -> R22
  23 -> R23
  24 -> R24
  25 -> R25
  26 -> R26
  27 -> R27
  28 -> R28
  29 -> R29
  30 -> R30
  31 -> R31

-- Get operation type from binary instructions.
-- e.g. 000000.. -> RType
decodeOp :: Unsigned 32 -> OpType
decodeOp x = case opPart of
               0 -> RType
               2 -> JumpType
               4 -> BranchType
               35 -> ReadMemType
               43 -> WriteMemType
               _ -> error "Unimplemented instruction(Op error)"
  where
    opPart :: BitVector 6
    opPart = slice d31 d26 (pack x)

-- Get function part from binary instructions.
-- e.g. ..100000 -> (+)
decodeFunct :: Unsigned 32 -> Unsigned 32 -> Unsigned 32 -> Unsigned 32
decodeFunct x = case slice d5 d0 (pack x) of
                  32 -> (+)
                  34 -> (-)
                  36 -> (and#)
                  37 -> (or#)
                  _ -> error "Unimplemented instruction(Funct error)"


-- Get what a register contains from a state.
-- The input data has 5-bit width because we have 32 registers.

-- ## This is interesting. Because SystemState can describe everything of our system (CPU and RAM),
-- ## we can just get anything we want (of the system) -- such as what a register contains.

-- To get the simplest implementation, we just take the lower 8 bits of certain register
{-
getOutput :: SystemState -> Maybe (Unsigned 32)
getOutput ((CPUState (Output x) _), _, _) = Just x
getOutput _ = Nothing
-}

getOutput :: Unsigned 5 -> SystemState -> Unsigned 8
getOutput n (CPUState _ regs, _, _) = unpack $ slice d7 d0 (pack (readRegister regs reg))
  where reg = decodeReg $ pack n


-- III. Main part that describes how to get a new state from the old one
cycle :: SystemState -> SystemState
cycle (CPUState activity regs, ram, tempRegs) = case activity of
  InstFetch -> (CPUState activity' regs', ram', tempRegs')
    where
      oldpc = pc regs
      newpc = increment oldpc
      tempRegs' = tempRegs { ir = readRAM ram oldpc }
      ram' = ram
      regs' = regs { pc = newpc }
      activity' = InstDecode

  InstDecode -> (CPUState activity' regs', ram', tempRegs')
    where
      -- [assist
      decodeRegA :: Unsigned 32 -> Register
      decodeRegA x = decodeReg $ slice d25 d21 (pack x)

      decodeRegB :: Unsigned 32 -> Register
      decodeRegB x = decodeReg $ slice d20 d16 (pack x)

      signExtend :: Unsigned 32 -> Unsigned 32
      signExtend x = unpack $ higherHalf ++# lowerHalf
        where lowerHalf = slice d15 d0 (pack x)
              higherHalf :: BitVector 16
              higherHalf = case msb lowerHalf of
                0 -> 0
                1 -> 65535
      -- assist]

      inst = ir tempRegs
      tempRegs' = tempRegs { ra = readRegister regs (decodeRegA inst)
                           , rb = readRegister regs (decodeRegB inst)
                           , aluout = (getPtrContents $ pc regs) + (signExtend inst) --  do not perform (* 4) because we only use word address
                           }
      activity' = Execute
      regs' = regs
      ram' = ram

  Execute -> (CPUState activity' regs', ram', tempRegs')
    where
      -- [assist
      jumpAddrProcess :: Unsigned 32 -> Unsigned 32 -> Unsigned 32
      jumpAddrProcess pc inst = unpack $ pcPart ++# irPart ++# constPart
        where pcPart = slice d31 d28 (pack pc) :: BitVector 4
              irPart = slice d25 d0 (pack inst) :: BitVector 26
              constPart = 0 :: BitVector 2
      -- assist]

      inst = ir tempRegs
      instType = decodeOp inst
      tempRegs' = case instType of
        RType -> tempRegs { aluout = (decodeFunct inst) (ra tempRegs) (rb tempRegs) }
        ReadMemType -> tempRegs { aluout = (ra tempRegs) + (signExtend inst) }
        WriteMemType -> tempRegs { aluout = (ra tempRegs) + (signExtend inst) }
        _ -> tempRegs
      regs' = case instType of
        BranchType -> regs { pc = if (ra tempRegs) == (rb tempRegs) then Ptr (aluout tempRegs) else pc regs }
        JumpType -> regs { pc = Ptr $ jumpAddrProcess (getPtrContents $ pc regs) inst }
        _ -> regs
      activity' = case instType of
        RType -> MemProcess
        ReadMemType -> MemProcess
        WriteMemType -> MemProcess
        _ -> InstFetch
      ram' = ram

  MemProcess -> (CPUState activity' regs', ram', tempRegs')
    where
      -- [assist
      decodeRd :: Unsigned 32 -> Register
      decodeRd x = decodeReg $ slice d15 d11 (pack x)
      -- assist]

      inst = ir tempRegs
      instType = decodeOp inst
      tempRegs' = case instType of
        ReadMemType -> tempRegs { mdr = readRAM ram (Ptr (aluout tempRegs)) }
        _ -> tempRegs
      regs' = case instType of
        RType -> writeRegister regs (decodeRd inst) (aluout tempRegs)
        _ -> regs
      ram' = case instType of
        WriteMemType -> writeRAM ram (Ptr (aluout tempRegs)) (rb tempRegs)
        _ -> ram
      activity' = case instType of
        ReadMemType -> FinishReadMem
        _ -> InstFetch

  FinishReadMem -> (CPUState activity' regs', ram', tempRegs')
    where
      -- [assist
      decodeRt :: Unsigned 32 -> Register
      decodeRt x = decodeReg $ slice d20 d16 (pack x)
      -- assist]

      inst = ir tempRegs
      instType = decodeOp inst
      tempRegs' = tempRegs
      regs' = writeRegister regs (decodeRt inst) (rb tempRegs)
      ram' = ram
      activity' = InstFetch


-- IV. Build a physical machine!
-- - So how to do it?
-- - You can remember the Merly model: OldState -> Input -> (NewState, Output),
--   and you know something called register which can store a state, then get a input and get a new one, and ...
--   All of them means that we just need to write a function :: s -> i -> (s, o),
--   as a Haskeller, I think you are very familiar with it :)

cpuHardware :: SystemState -> Unsigned 5 -> (SystemState, Unsigned 8)
cpuHardware s i = (s', getOutput i s')
  where s' = CPU.cycle s

-- We have not implemented file I/O, so just hard code the initial state (registers and RAM)
initSystemState :: SystemState
initSystemState = (CPUState initActivity initRegs, initRAM, initTempRegs)
  where initActivity = InstFetch
        initRegs = Registers { r0  = 0,
                               r1  = 0,
                               r2  = 0,
                               r3  = 0,
                               r4  = 0,
                               r5  = 0,
                               r6  = 0,
                               r7  = 0,
                               r8  = 0, -- $t0
                               r9  = 1, -- $t1
                               r10 = 0,
                               r11 = 0,
                               r12 = 0,
                               r13 = 0,
                               r14 = 0,
                               r15 = 0,
                               r16 = 0,
                               r17 = 0,
                               r18 = 0,
                               r19 = 0,
                               r20 = 0,
                               r21 = 0,
                               r22 = 0,
                               r23 = 0,
                               r24 = 0,
                               r25 = 0,
                               r26 = 0,
                               r27 = 0,
                               r28 = 0,
                               r29 = 0,
                               r30 = 0,
                               r31 = 0,
                               pc  = Ptr 0
                             }
        initTempRegs = TempRegs { ir  = 0,
                                  mdr = 0,
                                  ra  = 0,
                                  rb  = 0,
                                  aluout = 0
                                }
        initRAM = let a = 0x01094020 :: Unsigned 32 -- add $t0, $t0, $t1
                      r = 0x00004020 :: Unsigned 32 -- add $t0, $zero, $zero
                      j = 0x08000000 :: Unsigned 32 -- j 0x0
                  in RAM  (a :> a :> a :> a :> a :> a :> a :> a :>
                          a :> a :> a :> a :> a :> a :> a :> a :>
                          a :> a :> a :> a :> a :> a :> a :> a :>
                          a :> a :> a :> a :> a :> a :> a :> a :>
                          a :> a :> a :> a :> a :> a :> a :> a :>
                          a :> a :> a :> a :> a :> a :> a :> a :>
                          a :> a :> a :> a :> a :> a :> a :> a :>
                          a :> a :> a :> a :> a :> a :> r :> j :> Nil)

-- Clash can generate a mealy model for us
device = mealy cpuHardware initSystemState


-- Top Entity!
{-
exposeClockResetEnable means that we get
    input clk, rst, en
in compiled verilog code.
-}
{-# ANN topEntity
  (Synthesize
    { t_name   = "SimpleMCPU"
    , t_inputs = [PortName "clk", PortName "rst", PortName "en", PortName "SW"]
    , t_output = PortName "ALED"
    })#-}
topEntity
  :: Clock System
  -> Reset System
  -> Enable System
  -> Signal System (Unsigned 5)
  -> Signal System (Unsigned 8)
topEntity = exposeClockResetEnable device
