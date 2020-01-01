{-
  This is a VERY simple 32-bit MIPS CPU with 8 basic instructions(add, sub, and, or, lw, sw, beq and j).
  And it does not have a specialized RAM (instead we use a vector to simulate it).
  But if you want, you can implement them without much effort :)
-}
{-# LANGUAGE BangPatterns #-}

module CPU where

import Clash.Prelude
import Clash.Sized.Internal.Unsigned (and#, or#)
import Data.List as L hiding ((!!))
import Data.Maybe (isJust, fromJust)

-- I. Types that describe a CPU
{-
  Clash requires some of these types to be instances of Generic and NFDataX,
  which can be auto-derived.
-}

-- + pointer. It is just a 32 bit number and only used by pc
newtype Ptr = Ptr (Unsigned 32) deriving (Show, Eq, Generic, NFDataX)

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
                 | Nop
                 deriving (Show)

-- + Type of instructions.
data OpType = RType | BranchType | JumpType | ReadMemType | WriteMemType | NoType deriving (Show, Eq, Generic, NFDataX)


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
  } deriving (Generic, NFDataX)
instance Show Registers where
  show regs = (L.++) "\n " $
              (L.++) (L.concat $ L.map f [0..31]) $
              (L.++) (show $ getPtrContents $ pc regs)
              "\n "
    where f x = let additional = if x `mod` 8 == 7 then "\n " else " "
                in (L.++) (show $ readRegister regs (decodeReg (x :: BitVector 5))) additional

-- + RAM. It is just a 64-word vector
data RAM = RAM (Vec 64 (Unsigned 32)) deriving (Generic, NFDataX)
instance Show RAM where
  show (RAM ram) = (L.++) "\n " (L.concat $ L.map f [0..63])
    where f x = let additional = if x `mod` 8 == 7 then "\n " else " "
                in (L.++) (show $ ram !! x) additional

type IRAM = RAM
type DRAM = RAM

-- + Intermediate registers, for pipelining.
data IFID = IFID {
    ifidInstruction :: Unsigned 32,
    ifidNewpc :: Unsigned 32
    } deriving (Generic)
instance NFDataX IFID where
  rnfX !_ = ()
instance Show IFID where
  show ifid = (L.++) "\n \t ifidInstruction: " $
              (L.++) (show $ ifidInstruction ifid) $
              (L.++) "\n \t ifidNewpc: "
              (show $ ifidNewpc ifid)

data IDEX = IDEX {
    idexNewpc :: Unsigned 32,
    idexRegAData :: Unsigned 32,
    idexRegBData :: Unsigned 32,
    idexExtendImm :: Unsigned 32,
    idexSRegNum :: Unsigned 5,
    idexTRegNum :: Unsigned 5,
    idexDestRegNum :: Unsigned 5,
    idexInstruction :: Unsigned 32,
    idexStallStatus :: (Bool, Unsigned 2)
    } deriving (Generic)
instance NFDataX IDEX where
  rnfX !_ = ()
instance Show IDEX where
  show idex = (L.++) "\n \t idexNewpc: " $
              (L.++) (show $ idexNewpc idex) $
              (L.++) "\n \t idexRegAData: " $
              (L.++) (show $ idexRegAData idex) $
              (L.++) "\n \t idexRegBData: " $
              (L.++) (show $ idexRegBData idex) $
              (L.++) "\n \t idexExtendImm: " $
              (L.++) (show $ idexExtendImm idex) $
              (L.++) "\n \t idexSRegNum: " $
              (L.++) (show $ idexSRegNum idex) $
              (L.++) "\n \t idexTRegNum: " $
              (L.++) (show $ idexTRegNum idex) $
              (L.++) "\n \t idexDestRegNum: " $
              (L.++) (show $ idexDestRegNum idex) $
              (L.++) "\n \t idexInstruction: " $
              (L.++) (show $ idexInstruction idex) $
              (L.++) "\n \t idexStallStatus: "
              (show $ idexStallStatus idex)


data EXMEM = EXMEM {
    exmemJmpAddress :: Unsigned 32,
    exmemAluAns :: Unsigned 32,
    exmemWriteMemData :: Unsigned 32, -- regBData
    exmemBranchFlag :: Unsigned 1,
    exmemDestRegNum :: Unsigned 5, -- same in IDEX, EXMEM and MEMWB
    exmemInstType :: OpType
    } deriving (Generic)
instance NFDataX EXMEM where
  rnfX !_ = ()
instance Show EXMEM where
  show exmem = (L.++) "\n \t exmemJmpAddress: " $
               (L.++) (show $ exmemJmpAddress exmem) $
               (L.++) "\n \t exmemAluAns: " $
               (L.++) (show $ exmemAluAns exmem) $
               (L.++) "\n \t exmemWriteMemData: " $
               (L.++) (show $ exmemWriteMemData exmem) $
               (L.++) "\n \t exmemBranchFlag: " $
               (L.++) (show $ exmemBranchFlag exmem) $
               (L.++) "\n \t exmemDestRegNum: " $
               (L.++) (show $ exmemDestRegNum exmem) $
               (L.++) "\n \t exmemInstType: "
               (show $ exmemInstType exmem)

data MEMWB = MEMWB {
    memwbReadMemData :: Unsigned 32,
    memwbDestRegNum :: Unsigned 5,
    memwbInstType :: OpType
    } deriving (Generic)
instance NFDataX MEMWB where
  rnfX !_ = ()
instance Show MEMWB where
  show memwb = (L.++) "\n \t memwbReadMemData: " $
               (L.++) (show $ memwbReadMemData memwb) $
               (L.++) "\n \t memwbDestRegNum: " $
               (L.++) (show $ memwbDestRegNum memwb) $
               (L.++) "\n \t memwbInstType: "
               (show $ memwbInstType memwb)


data TempRegs = TempRegs {
    ifid :: IFID,
    idex :: IDEX,
    exmem :: EXMEM,
    memwb :: MEMWB
  } deriving (Generic)
instance NFDataX TempRegs where
  rnfX !_ = ()
instance Show TempRegs where
  show tempRegs = (L.++) "\n IFID: " $
                  (L.++) (show $ ifid tempRegs) $
                  (L.++) "\n IDEX: " $
                  (L.++) (show $ idex tempRegs) $
                  (L.++) "\n EXMEM: " $
                  (L.++) (show $ exmem tempRegs) $
                  (L.++) "\n MEMWB: "
                  (show $ memwb tempRegs)


-- + All the current information of our system, including CPU and RAM.
type SystemState = (IRAM, DRAM, Registers, TempRegs)

-- For Debug Use
diffSystemState :: SystemState -> SystemState -> IO ()
diffSystemState (RAM iram, RAM dram, regs, tempregs) (RAM iram', RAM dram', regs', tempregs') = do
  print "----------"
  iramDiff
  print "-----"
  dramDiff
  print "-----"
  regsDiff
  print "-----"
  diffTempRegs tempregs tempregs'
  print "----------"

  where iramDiffTuples = L.map fi $ L.filter (\x -> iram !! x /= iram' !! x) [0..63]
        dramDiffTuples = L.map fd $ L.filter (\x -> dram !! x /= dram' !! x) [0..63]
        fi x = (x, iram !! x, iram' !! x)
        fd x = (x, dram !! x, dram' !! x)

        readReg rs x = readRegister rs (decodeReg (x :: BitVector 5))
        regsDiffTuples = L.map fr $ L.filter (\x -> readReg regs x /= readReg regs' x) [0..31]
        fr x = (x, readReg regs x, readReg regs' x)

        iramDiff = do
          putStrLn "IRAM: "
          print iramDiffTuples

        dramDiff = do
          putStrLn "DRAM: "
          print dramDiffTuples

        regsDiff = do
          let additional = if pc regs == pc regs' then []
                           else [("pc", getPtrContents $ pc regs, getPtrContents $ pc regs')]
          putStrLn "Regs: "
          print regsDiffTuples
          print additional

diffTempRegs :: TempRegs -> TempRegs -> IO ()
diffTempRegs (TempRegs ifid idex exmem memwb) (TempRegs ifid' idex' exmem' memwb') = do
  diffIFID
  diffIDEX
  diffEXMEM
  diffMEMWB
  return ()
  where diffIFID = do
          putStrLn "IFID: "
          if ifidInstruction ifid /= ifidInstruction ifid' then print ("ifidInstruction", ifidInstruction ifid, ifidInstruction ifid') else putStr ""
          if ifidNewpc ifid /= ifidNewpc ifid' then print ("ifidNewpc", ifidNewpc ifid, ifidNewpc ifid') else putStr ""
        diffIDEX = do
          putStrLn "IDEX: "
          if idexNewpc idex /= idexNewpc idex' then print ("idexNewpc", idexNewpc idex, idexNewpc idex') else putStr ""
          if idexRegAData idex /= idexRegAData idex' then print ("idexRegAData", idexRegAData idex, idexRegAData idex') else putStr ""
          if idexRegBData idex /= idexRegBData idex' then print ("idexRegBData", idexRegBData idex, idexRegBData idex') else putStr ""
          if idexExtendImm idex /= idexExtendImm idex' then print ("idexExtendImm", idexExtendImm idex, idexExtendImm idex') else putStr ""
          if idexSRegNum idex /= idexSRegNum idex' then print ("idexSRegNum", idexSRegNum idex, idexSRegNum idex') else putStr ""
          if idexTRegNum idex /= idexTRegNum idex' then print ("idexTRegNum", idexTRegNum idex, idexTRegNum idex') else putStr ""
          if idexDestRegNum idex /= idexDestRegNum idex' then print ("idexDestRegNum", idexDestRegNum idex, idexDestRegNum idex') else putStr ""
          if idexInstruction idex /= idexInstruction idex' then print ("idexInstruction", idexInstruction idex, idexInstruction idex') else putStr ""
          if idexStallStatus idex /= idexStallStatus idex' then print ("idexStallStatus", idexStallStatus idex, idexStallStatus idex') else putStr ""
        diffEXMEM = do
          putStrLn "EXMEM: "
          if exmemJmpAddress exmem /= exmemJmpAddress exmem' then print ("exmemJmpAddress", exmemJmpAddress exmem, exmemJmpAddress exmem') else putStr ""
          if exmemAluAns exmem /= exmemAluAns exmem' then print ("exmemAluAns", exmemAluAns exmem, exmemAluAns exmem') else putStr ""
          if exmemWriteMemData exmem /= exmemWriteMemData exmem' then print ("exmemWriteMemData", exmemWriteMemData exmem, exmemWriteMemData exmem') else putStr ""
          if exmemBranchFlag exmem /= exmemBranchFlag exmem' then print ("exmemBranchFlag", exmemBranchFlag exmem, exmemBranchFlag exmem') else putStr ""
          if exmemDestRegNum exmem /= exmemDestRegNum exmem' then print ("exmemDestRegNum", exmemDestRegNum exmem, exmemDestRegNum exmem') else putStr ""
        diffMEMWB = do
          putStrLn "MEMWB: "
          if memwbReadMemData memwb /= memwbReadMemData memwb' then print ("memwbReadMemData", memwbReadMemData memwb, memwbReadMemData memwb') else putStr ""
          if memwbDestRegNum memwb /= memwbDestRegNum memwb' then print ("memwbDestRegNum", memwbDestRegNum memwb, memwbDestRegNum memwb') else putStr ""
          if memwbInstType memwb /= memwbInstType memwb' then print ("memwbInstType", memwbInstType memwb, memwbInstType memwb') else putStr ""


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
  _ -> R0 -- Default
-- Get operation type from binary instructions.
-- e.g. 000000.. -> RType
decodeOp :: Unsigned 32 -> OpType
decodeOp x = case opPart of
               0 -> RType
               2 -> JumpType
               4 -> BranchType
               35 -> ReadMemType
               43 -> WriteMemType
               63 -> NoType -- 11_1111
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
                  -- _ -> error "Unimplemented instruction(Funct error)"
                  _ -> (+)


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

getOutput :: Unsigned 5 -> SystemState -> Unsigned 32
getOutput n (_, _, regs, _) = readRegister regs reg
  --unpack $ slice d7 d0 (pack (readRegister regs reg))
  where reg = decodeReg $ pack n


-- III. Main part that describes how to get a new state from the old one
checkStall :: SystemState -> Bool
checkStall (_, _, _, TempRegs ifid idex exmem memwb) =
  tempIdexOpType == ReadMemType && (loadTo == readFroms || loadTo == readFromt)
  where tempIdexOpType = decodeOp $ idexInstruction idex
        tempStallStatus = idexStallStatus idex
        ifidOldInst = ifidInstruction ifid
        loadTo = idexDestRegNum idex
        readFroms = unpack $ slice d25 d21 (pack ifidOldInst)
        readFromt = unpack $ slice d20 d16 (pack ifidOldInst)

stall :: SystemState -> SystemState
stall s = (iram',dram',regs',tempregs'')
  where (iram',dram',regs',tempregs') = exProcess (fwd1,fwd2) . memProcess . wbProcess $ s
        idex' = idex tempregs'
        tempregs'' = tempregs' { idex = idex' { idexInstruction = 4227858432 } } -- insert a Nop
        fwd1 = checkForward1 s
        fwd2 = checkForward2 s

plainCycle :: SystemState -> SystemState
plainCycle s = ifProcess . idProcess . exProcess (fwd1,fwd2) . memProcess . wbProcess $ s
  where fwd1 = checkForward1 s
        fwd2 = checkForward2 s

cycle :: SystemState -> SystemState
cycle s = if checkStall s == True then
          let (_,_,_,tempregs) = s in
            case idexStallStatus $ idex tempregs of
              (False, 0) -> let (iram',dram',regs',tempregs') = stall s
                                idex'' = (idex tempregs') { idexStallStatus = (True,0) }
                                tempregs'' = tempregs' { idex = idex'' }
                            in (iram',dram',regs',tempregs'')
              _ -> error "Stall error"
          else plainCycle s

applyN :: Int -> (a -> a) -> a -> a
applyN = (L.foldr (.) id.) . L.replicate

runCycle :: SystemState -> Int -> SystemState
runCycle s n = applyN n CPU.cycle s

runCycleWithDiff :: Int -> IO ()
runCycleWithDiff 0 = do
  putStrLn "\n******* 0 *******"
  print initSystemState
  print "\n\n"
runCycleWithDiff n = do
  runCycleWithDiff (n-1)
  putStrLn $ (L.++) "\n******* " $
             (L.++) (show n)
             " *******\n"
  diffSystemState (runCycle initSystemState (n-1)) (runCycle initSystemState n)


-------- Forwarding

isRegWriteType :: OpType -> Bool
isRegWriteType RType = True
isRegWriteType ReadMemType = True
isRegWriteType _ = False

data ForwardTarget = FWDA
                   | FWDB
                   | FWDAB
                   deriving (Show, Eq)

checkForward1 :: SystemState -> Maybe ForwardTarget
checkForward1 (_,_,_,TempRegs ifid idex exmem memwb)
  | isW == False = Nothing
  | eqS     && not eqT = Just FWDA
  | eqS     && eqT     = Just FWDAB
  | not eqS && eqT     = Just FWDB
  | otherwise          = Nothing
  where isW = isRegWriteType $ exmemInstType exmem
        typeST = decodeOp $ idexInstruction idex
        eqS = exmemDestRegNum exmem == idexSRegNum idex && typeST /= NoType
        eqT = exmemDestRegNum exmem == idexTRegNum idex && typeST /= NoType

checkForward2 :: SystemState -> Maybe ForwardTarget
checkForward2 (_,_,_,TempRegs ifid idex exmem memwb)
  | isW == False = Nothing
  | eqS     && not eqT && not eqSExM               = Just FWDA
  | not eqS && eqT     && not eqTExM               = Just FWDB
  | eqS     && eqT     && not eqSExM && not eqTExM = Just FWDAB
  | otherwise                                      = Nothing
  where isW = isRegWriteType $ memwbInstType memwb
        typeST = decodeOp $ idexInstruction idex
        typeSTExM = exmemInstType exmem
        eqS = memwbDestRegNum memwb == idexSRegNum idex && typeST /= NoType
        eqT = memwbDestRegNum memwb == idexTRegNum idex && typeST /= NoType
        eqSExM = exmemDestRegNum exmem == idexSRegNum idex && typeSTExM /= NoType
        eqTExM = exmemDestRegNum exmem == idexTRegNum idex && typeSTExM /= NoType



wbProcess :: SystemState -> SystemState
wbProcess (iram, dram, regs, tempRegs) = (iram', dram', regs', tempRegs')
    where iram' = iram
          dram' = dram
          oldMemwb = memwb tempRegs
          regs' = case memwbInstType oldMemwb of
                    RType       -> writeRegister regs (decodeReg $ pack (memwbDestRegNum oldMemwb)) (memwbReadMemData oldMemwb) -- same as ReadMemType
                    ReadMemType -> writeRegister regs (decodeReg $ pack (memwbDestRegNum oldMemwb)) (memwbReadMemData oldMemwb)
                    _ -> regs -- Nop included
          tempRegs' = tempRegs


memProcess :: SystemState -> SystemState
memProcess (iram, dram, regs, tempRegs) = (iram', dram', regs', tempRegs')
  where iram' = iram
        dram' = case exmemInstType oldExmem of
                  WriteMemType -> writeRAM dram (Ptr $ exmemAluAns oldExmem) (exmemWriteMemData oldExmem)
                  _ -> dram
        regs' = case exmemInstType oldExmem of
                  NoType -> regs
                  _ ->
                    regs { pc = if exmemBranchFlag oldExmem == 1
                                then Ptr $ exmemJmpAddress oldExmem
                                else pc regs }
                                --increment $ pc regs }
        memwb' = MEMWB {
          memwbReadMemData = case exmemInstType oldExmem of
                               ReadMemType -> readRAM dram (Ptr $ exmemAluAns oldExmem)
                               RType -> exmemAluAns oldExmem
                               _ -> memwbReadMemData $ memwb tempRegs
--          , memwbAluAns = exmemAluAns oldExmem
          , memwbDestRegNum = exmemDestRegNum oldExmem
          , memwbInstType = exmemInstType oldExmem -- Nop included
          }
        -- Predict not taken
        ifid' = oldIfid {
          ifidInstruction = if exmemBranchFlag oldExmem == 1 then 4227858432 else ifidInstruction oldIfid
          }
        idex' = oldIdex {
          idexInstruction = if exmemBranchFlag oldExmem == 1 then 4227858432 else idexInstruction oldIdex
          }
        -- end
        tempRegs' = tempRegs {
            ifid = ifid'
          , idex = idex'
          , memwb = memwb'
          }
        oldIfid = ifid tempRegs
        oldIdex = idex tempRegs
        oldExmem = exmem tempRegs


exProcess :: (Maybe ForwardTarget, Maybe ForwardTarget) -> SystemState -> SystemState
exProcess  (fwd1,fwd2) s@(iram, dram, regs, tempRegs) = (iram', dram', regs', tempRegs')
  where iram' = iram
        dram' = dram
        regs' = regs
        oldOpType = decodeOp $ idexInstruction oldIdex
        exmem' = EXMEM {
          exmemJmpAddress = case oldOpType of
                         JumpType ->
                           unpack $ (0 :: BitVector 6) ++# (slice d25 d0 (pack $ idexInstruction oldIdex))
                         BranchType ->
                           (idexNewpc oldIdex) + (idexExtendImm oldIdex)
                         _ -> 0
          , exmemWriteMemData = idexRegBData oldIdex
          , exmemInstType = oldOpType -- Nop included
          , exmemDestRegNum = idexDestRegNum oldIdex
          , exmemBranchFlag = case oldOpType of
                         JumpType -> 1
                         BranchType -> if (idexSRegNum oldIdex) == (idexTRegNum oldIdex)
                                       then 1 else 0
                         _ -> 0
          , exmemAluAns = case oldOpType of
                     -- [modified because of forwarding
                     RType -> (decodeFunct $ idexInstruction oldIdex) realRegAData realRegBData
                     ReadMemType -> (idexExtendImm oldIdex) + realRegAData
                     WriteMemType -> (idexExtendImm oldIdex) + realRegAData
                     -- ]
                     _ -> 0
          }
        tempRegs' = tempRegs { exmem = exmem' }
        oldIdex = idex tempRegs
        oldMemwb = memwb tempRegs
        -- forwarding
        realRegAData
          | isJust fwd1 && ((fromJust fwd1 == FWDA) || (fromJust fwd1 == FWDAB)) = memwbReadMemData oldMemwb
          | isJust fwd2 && ((fromJust fwd2 == FWDA) || (fromJust fwd2 == FWDAB)) = memwbReadMemData oldMemwb
          | otherwise = idexRegAData oldIdex

        realRegBData
          | isJust fwd1 && ((fromJust fwd1 == FWDB) || (fromJust fwd1 == FWDAB)) = memwbReadMemData oldMemwb
          | isJust fwd1 && ((fromJust fwd1 == FWDB) || (fromJust fwd1 == FWDAB)) = memwbReadMemData oldMemwb
          | otherwise = idexRegBData oldIdex



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


idProcess :: SystemState -> SystemState
idProcess (iram, dram, regs, tempRegs) = (iram', dram', regs', tempRegs')
  where iram' = iram
        dram' = dram
        regs' = regs
        oldInst = ifidInstruction $ ifid tempRegs
        idex' = IDEX {
          idexNewpc = ifidNewpc $ ifid tempRegs
          , idexRegAData = readRegister regs (decodeRegA oldInst)
          , idexRegBData = readRegister regs (decodeRegB oldInst)
          , idexExtendImm = CPU.signExtend oldInst
          , idexSRegNum = unpack $ slice d25 d21 (pack oldInst)
          , idexTRegNum = unpack $ slice d20 d16 (pack oldInst)
          , idexDestRegNum = case decodeOp oldInst of
                         RType -> unpack $ slice d15 d11 (pack oldInst)
                         ReadMemType -> unpack $ slice d20 d16 (pack oldInst)
                         _ -> 0
          , idexInstruction = oldInst -- Nop included
          , idexStallStatus = (False, 0)
          }
        tempRegs' = tempRegs { idex = idex' }

ifProcess :: SystemState -> SystemState
ifProcess (iram, dram, regs, tempRegs) = (iram', dram', regs', tempRegs')
  where iram' = iram
        dram' = dram
        newPC = getPtrContents (increment $ pc regs)
        regs' = regs { pc = Ptr newPC}
        ifid' = IFID {
          ifidInstruction = readRAM iram (pc regs)
          , ifidNewpc = newPC
          }
        tempRegs' = tempRegs { ifid = ifid' }

-- IV. Build a physical machine!
-- - So how to do it?
-- - You can remember the Merly model: OldState -> Input -> (NewState, Output),
--   and you know something called register which can store a state, then get a input and get a new one, and ...
--   All of them means that we just need to write a function :: s -> i -> (s, o),
--   as a Haskeller, I think you are very familiar with it :)


cpuHardware :: SystemState -> Unsigned 5 -> (SystemState, Unsigned 32)
cpuHardware s i = (s', getOutput i s')
  where s' = CPU.cycle s

-- We have not implemented file I/O, so just hard code the initial state (registers and RAM)
initSystemState :: SystemState
initSystemState = (initIRAM, initDRAM, initRegs, initTempRegs)
  where initRegs = Registers { r0  = 0,
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
        initTempRegs = TempRegs {
          ifid = IFID 4227858432 0
          , idex = IDEX 0 0 0 0 0 0 0 4227858432 (False,0)
          , exmem = EXMEM 0 0 0 0 0 NoType
          , memwb = MEMWB 0 0 NoType
          }
        initIRAM = let a = 0x01094020 :: Unsigned 32 -- add $t0, $t0, $t1
                       r = 0x00004020 :: Unsigned 32 -- add $t0, $zero, $zero
                       j = 0x08000000 :: Unsigned 32 -- j 0x0

                       lwt_0 = 0x8c080000 :: Unsigned 32 -- lw $t0, 0($zero)
                       j_2 = 0x08000002 :: Unsigned 32 -- j 0x2

                       lwt_2 = 0x8e0a0000 :: Unsigned 32 -- lw $t2, 0($s0)
                       lwt_3 = 0x8e0b0000 :: Unsigned 32 -- lw $t3, 0($s0)
                       lwt_4 = 0x8e0c0000 :: Unsigned 32 -- lw $t4, 0($s0)
                       lwt_5 = 0x8e0d0000 :: Unsigned 32 -- lw $t5, 0($s0)
                       lwt_6 = 0x8e0e0000 :: Unsigned 32 -- lw $t6, 0($s0)
                       lwt_7 = 0x8e0f0000 :: Unsigned 32 -- lw $t7, 0($s0)
                   in RAM (lwt_0 :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> j :>
                           lwt_0 :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> j :> Nil)

        initDRAM = let a = 0x00000000 :: Unsigned 32 -- const 0
                       b = 0x00001145 :: Unsigned 32 -- const 0x1145
                   in RAM (b :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :>
                           a :> a :> a :> a :> a :> a :> a :> a :> Nil)

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
