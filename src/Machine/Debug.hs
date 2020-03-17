{-# LANGUAGE NoImplicitPrelude #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Machine.Debug
-- Copyright   :  (c) Commelina 2020
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  maosics@gmail.com
-- Stability   :  unstable
--
-- V. Some extra functions for debug usage.
-----------------------------------------------------------------------------


module Machine.Debug
       (
         plainCycle
       , cycle
       , runCycle
       ) where

import Control.Lens hiding ((:>))
import Prelude hiding (cycle)

import Machine.Process
import Machine.Assist
import Machine.Types


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
runCycle s n = applyN n cycle s
