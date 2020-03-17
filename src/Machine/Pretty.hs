{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Machine.Pretty
-- Copyright   :  (c) Commelina 2020
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  maosics@gmail.com
-- Stability   :  unstable
--
-- II. Instance declarations for 'Pretty' and 'Show' typeclasses.
-- For better log info.
-----------------------------------------------------------------------------

module Machine.Pretty where

import Clash.Prelude hiding ((<>), empty, tail, (++), cycle)
import Control.Lens hiding ((:>))
import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Machine.Types
import Machine.Assist

instance Pretty (Unsigned n) where
    pPrint = text . show

instance Pretty OpType where
    pPrint t = text (show t)


instance Pretty Registers where
    pPrint r = text "Common-use regs: " $$
               nest 4 (fsep cDocs)      $$
               text "PC: " <> pcDoc
      where
        cDocs = pPrint <$> cutSegs (toList $ cRegs r) 8
        pcDoc = pPrint (pc r)
instance Show Registers where
    show = render . pPrint


instance Pretty RAM where
    pPrint r = nest 4 (fsep docs)
      where
        docs = pPrint <$> cutSegs (toList $ unRAM r) 8
instance Show RAM where
  show = render . pPrint


instance Pretty IFID where
    pPrint ifid = nest 4 $ vcat
      [ text "ifidInst: "  <> pPrint (ifid ^. ifidInst)
      , text "ifidNewpc: " <> pPrint (ifid ^. ifidNewpc)
      ]

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

instance Pretty EXMEM where
    pPrint exmem = nest 4 $ vcat
      [ text "exmemJAddr: "    <> pPrint (exmem ^. exmemJAddr)
      , text "exmemALU: "      <> pPrint (exmem ^. exmemALU)
      , text "exmemWMData: "   <> pPrint (exmem ^. exmemWMData)
      , text "exmemBrch: "     <> pPrint (exmem ^. exmemBrch)
      , text "exmemDNO: "      <> pPrint (exmem ^. exmemDNO)
      , text "exmemInstType: " <> pPrint (exmem ^. exmemInstType)
      ]

instance Pretty MEMWB where
    pPrint memwb = nest 4 $ vcat
      [ text "memwbRMData: "   <> pPrint (memwb ^. memwbRMData)
      , text "memwbDNO: "      <> pPrint (memwb ^. memwbDNO)
      , text "memwbInstType: " <> pPrint (memwb ^. memwbInstType)
      ]


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


instance Pretty SysState where
    pPrint (SysState i d r t) =
        text "IRAM: " $$ pPrint i $$
        text "DRAM: " $$ pPrint d $$
        pPrint r $$
        pPrint t
instance Show SysState where
    show = render . pPrint
