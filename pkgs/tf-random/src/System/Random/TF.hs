-- |
-- Module    : System.Random.TF
-- Copyright : (c) 2012-2013 Michał Pałka
-- License   : BSD3
--
-- Maintainer  : michal.palka@chalmers.se
-- Stability   : experimental
-- Portability : portable
--
-- This module exports "System.Random.TF.Gen" and "System.Random.TF.Init"
-- modules without exporting the alternative 'System.Random.TF.Gen.RandomGen'
-- class from "System.Random.TF.Gen". To use this class and the 'System.Random.TF.Instances.Random'
-- instances written for it, please import "System.Random.TF.Gen" and "System.Random.TF.Instances"
-- directly.

module System.Random.TF (
  module System.Random.TF.Gen,
  module System.Random.TF.Init,
  )
  where

import System.Random.TF.Gen hiding (RandomGen (..))
import System.Random.TF.Init hiding (initTFGen)

