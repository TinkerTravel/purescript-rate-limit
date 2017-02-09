module Test.Main
  ( main
  ) where

import Control.Monad.Aff (later')
import Control.Monad.Eff.Class (liftEff)
import Control.Parallel (parTraverse_)
import Data.RateLimit as RateLimit
import Data.Time.Duration (Milliseconds(..))
import Prelude
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

main = runTest do
  test "Data.RateLimit" do
    let policy1 = RateLimit.Policy (Milliseconds 100.0) 2
    let policy2 = RateLimit.Policy (Milliseconds 150.0) 3

    state <- liftEff $ RateLimit.newState [policy1, policy2]

    flip parTraverse_ [false, true] \key -> do
      liftEff $ RateLimit.recordAccess state key
      liftEff $ RateLimit.recordAccess state key
      Assert.assert "checkAccess" =<< liftEff (RateLimit.checkAccess state key)

      liftEff $ RateLimit.recordAccess state key
      Assert.assertFalse "checkAccess" =<< liftEff (RateLimit.checkAccess state key)

      later' 100 (pure unit)
      Assert.assert "checkAccess" =<< liftEff (RateLimit.checkAccess state key)

      liftEff $ RateLimit.recordAccess state key
      Assert.assertFalse "checkAccess" =<< liftEff (RateLimit.checkAccess state key)

      later' 50 (pure unit)
      Assert.assert "checkAccess" =<< liftEff (RateLimit.checkAccess state key)
