module Main where

import qualified Network.Api.Pin.Tests
import Test.Framework

main ::
  IO ()
main =
  defaultMain tests

tests ::
  [Test]
tests =
  [
    testGroup "Tests"
      [
        Pin.Tests.test
      ]
  ]
