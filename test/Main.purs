module Test.Main where

import Prelude (Unit, ($), bind, (<>), (>>=), (<$>), (/=), (<<<))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Test.Unit (TestSuite, test, suite)
import Data.Either (Either(..))
import Data.Argonaut.Parser as AP
import Test.Unit.Assert as A
import Data.Maybe (Maybe(..))
import Effect.Class (liftEffect)
import Data.String.Utils (lines, trimEnd)
import Test.Unit.Main (runTest)
import Data.Traversable (traverse_)
import Data.Array as Arr

import Sequent as Seq
import Json (fromJson)
import Proof as P

testFile :: String -> TestSuite
testFile location = test location $ do
    jsonProof <- liftEffect $ readTextFile UTF8 $
        "test/" <> location <> "_proof.json"
    expected <- trimEnd <$> liftEffect
        (readTextFile UTF8 $ "test/" <> location <> "_expected")
    let proofOutput = case AP.jsonParser jsonProof >>= fromJson of
            Left e -> "Error: " <> e
            Right p -> case P.conclusion p.proof of
                Just s -> "Success: " <> Seq.render s
                Nothing -> "Error: Failed to generate sequent"
    A.equal expected proofOutput

main :: Effect Unit
main = do
    testlist <- Arr.filter (_ /= "") <<< lines <$>
        liftEffect (readTextFile UTF8 "test/testlist")
    runTest $ suite "Tests from files" $ traverse_ testFile testlist
