{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.Text             as T
import qualified Ide.Plugin.Strictness as Strictness
import           System.FilePath       ((</>))
import           Test.Hls


main :: IO ()
main = defaultTestRunner test

plugin :: PluginTestDescriptor Strictness.Log
plugin = mkPluginTestDescriptor Strictness.descriptor "strictness"

test :: TestTree
test = testGroup "strictness"
  [ onlyRunForGhcVersions [GHC810, GHC90] "Demand signature format changed between GHC9.0-9.2" $ testGroup "old format"
       [ testGroup "simple"
           [ hoverTest "signature" (Position 4 1) "<S,1*U(U)><S(S),1*U(1*U)>"
           , hoverTest "first branch" (Position 5 1) "<S,1*U(U)><S(S),1*U(1*U)>"
           , hoverTest "second branch" (Position 6 1) "<S,1*U(U)><S(S),1*U(1*U)>"
           ]
       ]
  , onlyRunForGhcVersions [GHC92, GHC94] "Demand signature format changed between GHC9.0-9.2" $ testGroup "new format"
       [ testGroup "simple"
           [ hoverTest "signature" (Position 4 1) "<1!P(L)><1!P(1L)>"
           , hoverTest "first branch" (Position 5 1) "<1!P(L)><1!P(1L)>"
           , hoverTest "second branch" (Position 6 1) "<1!P(L)><1!P(1L)>"
           ]
       ]
  ]

-- Adapted from: plugins/hls-explicit-fixity-plugin/test/Main.hs
hoverTest :: TestName -> Position -> T.Text -> TestTree
hoverTest title pos expected =
  testCase title $ runSessionWithServer plugin testDataDir $ do
    doc <- openDoc "Sigs.hs" "haskell"
    waitForKickDone
    h <- getHover doc pos
    case h of
      Nothing -> liftIO $ assertFailure "No hover"
      Just (Hover contents _) -> case contents of
        HoverContentsMS _ ->
          liftIO $ assertFailure "Unexpected content type"
        HoverContents (MarkupContent _ txt) ->
          liftIO $ assertBool (errMsg txt) $ expected `T.isInfixOf` txt
    closeDoc doc
  where
    errMsg txt = T.unpack $ "Failed to find `" <> expected <> "` in hover message: " <> txt

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
