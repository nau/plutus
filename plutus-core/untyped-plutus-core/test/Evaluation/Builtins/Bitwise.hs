{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# OPTIONS_GHC -Werror #-}

module Evaluation.Builtins.Bitwise (
  bitwiseAndCommutes,
  bitwiseIorCommutes,
  bitwiseXorCommutes,
  bitwiseAndIdentity,
  bitwiseIorIdentity,
  bitwiseXorIdentity,
  bitwiseAndAbsorbing,
  bitwiseIorAbsorbing,
  bitwiseXorComplement,
  bitwiseAndSelf,
  bitwiseIorSelf,
  bitwiseXorSelf,
  ) where

import Control.Lens.Fold (Fold, folding, has, hasn't, preview)
import Data.Bitraversable (bitraverse)
import Data.Bits (complement, xor, zeroBits, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Word (Word8)
import Evaluation.Builtins.Common (typecheckEvaluateCek)
import GHC.Exts (fromListN)
import Hedgehog (Gen, PropertyT, Range, annotate, cover, evalEither, failure, forAllWith, success, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PlutusCore (DefaultFun (AndByteString, ComplementByteString, IorByteString, XorByteString), DefaultUni,
                   EvaluationResult (EvaluationFailure, EvaluationSuccess), Name, Term)
import PlutusCore.Evaluation.Machine.ExBudgetingDefaults (defaultCekParameters)
import PlutusCore.MkPlc (builtin, mkConstant, mkIterApp)
import Text.Show.Pretty (ppShow)
import UntypedPlutusCore qualified as Untyped

bitwiseIorCommutes :: PropertyT IO ()
bitwiseIorCommutes = commutative (.|.) IorByteString

bitwiseAndCommutes :: PropertyT IO ()
bitwiseAndCommutes = commutative (.&.) AndByteString

bitwiseXorCommutes :: PropertyT IO ()
bitwiseXorCommutes = commutative xor XorByteString

bitwiseAndIdentity :: PropertyT IO ()
bitwiseAndIdentity = identity (complement zeroBits) AndByteString

bitwiseIorIdentity :: PropertyT IO ()
bitwiseIorIdentity = identity zeroBits IorByteString

bitwiseXorIdentity :: PropertyT IO ()
bitwiseXorIdentity = identity zeroBits XorByteString

bitwiseAndAbsorbing :: PropertyT IO ()
bitwiseAndAbsorbing = absorbing zeroBits AndByteString

bitwiseIorAbsorbing :: PropertyT IO ()
bitwiseIorAbsorbing = absorbing (complement zeroBits) IorByteString

bitwiseXorComplement :: PropertyT IO ()
bitwiseXorComplement = do
  bs <- forAllWith ppShow . Gen.bytes $ byteBoundRange
  let len = BS.length bs
  let allOnes = BS.replicate len . complement $ zeroBits
  outcome1 <- goXor bs allOnes
  outcome2 <- goComplement bs
  case (outcome1, outcome2) of
    (EvaluationSuccess res1, EvaluationSuccess res2) -> res1 === res2
    _                                                -> failure
  where
    goXor ::
      ByteString ->
      ByteString ->
      PropertyT IO (EvaluationResult (Untyped.Term Name DefaultUni DefaultFun ()))
    goXor leftArg rightArg = do
      let leftArg' = mkConstant @ByteString () leftArg
      let rightArg' = mkConstant @ByteString () rightArg
      let comp = mkIterApp () (builtin () XorByteString) [leftArg', rightArg']
      cekEval comp
    goComplement ::
      ByteString ->
      PropertyT IO (EvaluationResult (Untyped.Term Name DefaultUni DefaultFun ()))
    goComplement bs = do
      let bs' = mkConstant @ByteString () bs
      let comp = mkIterApp () (builtin () ComplementByteString) [bs']
      cekEval comp

bitwiseAndSelf :: PropertyT IO ()
bitwiseAndSelf = self AndByteString

bitwiseIorSelf :: PropertyT IO ()
bitwiseIorSelf = self IorByteString

bitwiseXorSelf :: PropertyT IO ()
bitwiseXorSelf = do
  bs <- forAllWith ppShow . Gen.bytes $ byteBoundRange
  let len = BS.length bs
  let bs' = mkConstant @ByteString () bs
  let expected = mkConstant @ByteString () . BS.replicate len $ zeroBits
  let comp = mkIterApp () (builtin () XorByteString) [bs', bs']
  outcome <- cekEval comp
  case outcome of
    EvaluationSuccess res -> res === expected
    _                     -> failure

-- Helpers

self :: DefaultFun -> PropertyT IO ()
self b = do
  bs <- forAllWith ppShow . Gen.bytes $ byteBoundRange
  let bs' = mkConstant @ByteString () bs
  let comp = mkIterApp () (builtin () b) [bs', bs']
  outcome <- cekEval comp
  case outcome of
    EvaluationSuccess res -> res === mkConstant @ByteString () bs
    _                     -> failure

data AbsorbingCase =
  AbsorbingMismatched ByteString Int Word8 |
  AbsorbingMatched ByteString Word8
  deriving stock (Eq, Show)

_AbsorbingResult :: Fold AbsorbingCase ByteString
_AbsorbingResult = folding $ \case
  AbsorbingMatched bs w8 -> pure . BS.replicate (BS.length bs) $ w8
  _                      -> Nothing

getAbsorbingArgs :: AbsorbingCase -> (ByteString, ByteString)
getAbsorbingArgs = \case
  AbsorbingMismatched bs len w8 -> (bs, BS.replicate len w8)
  AbsorbingMatched bs w8        -> (bs, BS.replicate (BS.length bs) w8)

absorbing ::
  Word8 ->
  DefaultFun ->
  PropertyT IO ()
absorbing w8 b = do
  testCase <- forAllWith ppShow . genAbsorbingCase $ w8
  cover 45 "mismatched lengths" . hasn't _AbsorbingResult $ testCase
  cover 45 "matched lengths" . has _AbsorbingResult $ testCase
  let expectedMay = preview _AbsorbingResult testCase
  let (leftArg, rightArg) = getAbsorbingArgs testCase
  outcome <- commutatively b leftArg rightArg
  case (outcome, expectedMay) of
    ((EvaluationFailure, EvaluationFailure), Nothing) -> success
    (_, Nothing) -> do
      annotate "Unexpected success"
      failure
    ((EvaluationSuccess l2r, EvaluationSuccess r2l), Just expected) -> do
      l2r === r2l
      l2r === mkConstant () expected
    _ -> do
      annotate "Unexpected failure"
      failure

data IdentityCase =
  IdentityMismatched ByteString Int Word8 |
  IdentityMatched ByteString Word8
  deriving stock (Eq, Show)

_IdentityResult :: Fold IdentityCase ByteString
_IdentityResult = folding $ \case
  IdentityMatched res _ -> pure res
  _                     -> Nothing

getIdentityArgs :: IdentityCase -> (ByteString, ByteString)
getIdentityArgs = \case
  IdentityMismatched bs len w8 -> (bs, BS.replicate len w8)
  IdentityMatched bs w8        -> (bs, BS.replicate (BS.length bs) w8)

identity ::
  Word8 ->
  DefaultFun ->
  PropertyT IO ()
identity w8 b = do
  testCase <- forAllWith ppShow . genIdentityCase $ w8
  cover 45 "mismatched lengths" . hasn't _IdentityResult $ testCase
  cover 45 "matched lengths" . has _IdentityResult $ testCase
  let expectedMay = preview _IdentityResult testCase
  let (leftArg, rightArg) = getIdentityArgs testCase
  outcome <- commutatively b leftArg rightArg
  case (outcome, expectedMay) of
    ((EvaluationFailure, EvaluationFailure), Nothing) -> success
    (_, Nothing) -> do
      annotate "Unexpected success"
      failure
    ((EvaluationSuccess l2r, EvaluationSuccess r2l), Just expected) -> do
      l2r === r2l
      l2r === mkConstant () expected
    _ -> do
      annotate "Unexpected failure"
      failure

data CommutativeCase =
  MismatchedLengths ByteString ByteString |
  MatchedLengths ByteString ByteString ByteString
  deriving stock (Eq, Show)

getArgs :: CommutativeCase -> (ByteString, ByteString)
getArgs = \case
  MismatchedLengths bs bs' -> (bs, bs')
  MatchedLengths bs bs' _  -> (bs, bs')

_CommutativeResult :: Fold CommutativeCase ByteString
_CommutativeResult = folding $ \case
  MatchedLengths _ _ res -> pure res
  _                      -> Nothing

commutative ::
  (Word8 -> Word8 -> Word8) ->
  DefaultFun ->
  PropertyT IO ()
commutative f b = do
  testCase <- forAllWith ppShow . genCommutativeCase $ f
  cover 45 "mismatched lengths" . hasn't _CommutativeResult $ testCase
  cover 45 "matched lengths" . has _CommutativeResult $ testCase
  let expectedMay = preview _CommutativeResult testCase
  let (leftArg, rightArg) = getArgs testCase
  outcome <- commutatively b leftArg rightArg
  case (outcome, expectedMay) of
    ((EvaluationFailure, EvaluationFailure), Nothing) -> success
    (_, Nothing) -> do
      annotate "Unexpected success"
      failure
    ((EvaluationSuccess l2r, EvaluationSuccess r2l), Just expected) -> do
      l2r === r2l
      l2r === mkConstant () expected
    _ -> do
      annotate "Unexpected failure"
      failure

commutatively ::
  DefaultFun ->
  ByteString ->
  ByteString ->
  PropertyT IO (EvaluationResult (Untyped.Term Name DefaultUni DefaultFun ()),
                EvaluationResult (Untyped.Term Name DefaultUni DefaultFun ()))
commutatively fun leftArg rightArg = do
  let leftArg' = mkConstant @ByteString () leftArg
  let rightArg' = mkConstant @ByteString () rightArg
  let oneDirection = go leftArg' rightArg'
  let otherDirection = go rightArg' leftArg'
  bitraverse cekEval cekEval (oneDirection, otherDirection)
  where
    go :: Term Untyped.TyName Name DefaultUni DefaultFun () ->
          Term Untyped.TyName Name DefaultUni DefaultFun () ->
          Term Untyped.TyName Name DefaultUni DefaultFun ()
    go arg1 arg2 = mkIterApp () (builtin () fun) [arg1, arg2]

cekEval ::
  Term Untyped.TyName Name DefaultUni DefaultFun () ->
  PropertyT IO (EvaluationResult (Untyped.Term Name DefaultUni DefaultFun ()))
cekEval = fmap fst . evalEither . typecheckEvaluateCek defaultCekParameters

-- Generators

genCommutativeCase :: (Word8 -> Word8 -> Word8) -> Gen CommutativeCase
genCommutativeCase f = Gen.choice [mismatched, matched]
  where
    mismatched :: Gen CommutativeCase
    mismatched = do
      leftArg <- Gen.bytes byteBoundRange
      rightArg <- Gen.bytes byteBoundRange
      if BS.length leftArg /= BS.length rightArg
      then pure . MismatchedLengths leftArg $ rightArg
      else do
        let diff = BS.length leftArg - BS.length rightArg
        extension <- Gen.bytes . diffRange $ diff
        let leftArg' = leftArg <> extension
        Gen.element [MismatchedLengths leftArg' rightArg,
                     MismatchedLengths rightArg leftArg']
    matched :: Gen CommutativeCase
    matched = do
      leftArg <- Gen.bytes byteBoundRange
      let len = BS.length leftArg
      rightArg <- Gen.bytes . Range.singleton $ len
      let result = fromListN len . BS.zipWith f leftArg $ rightArg
      pure . MatchedLengths leftArg rightArg $ result

genIdentityCase :: Word8 -> Gen IdentityCase
genIdentityCase w8 = Gen.choice [mismatched, matched]
  where
    mismatched :: Gen IdentityCase
    mismatched = do
      bs <- Gen.bytes byteBoundRange
      let len = BS.length bs
      genLen <- Gen.filter (/= len) . Gen.int $ byteBoundRange
      pure . IdentityMismatched bs genLen $ w8
    matched :: Gen IdentityCase
    matched = do
      bs <- Gen.bytes byteBoundRange
      pure . IdentityMatched bs $ w8

genAbsorbingCase :: Word8 -> Gen AbsorbingCase
genAbsorbingCase w8 = Gen.choice [mismatched, matched]
  where
    mismatched :: Gen AbsorbingCase
    mismatched = do
      bs <- Gen.bytes byteBoundRange
      let len = BS.length bs
      genLen <- Gen.filter (/= len) . Gen.int $ byteBoundRange
      pure . AbsorbingMismatched bs genLen $ w8
    matched :: Gen AbsorbingCase
    matched = do
      bs <- Gen.bytes byteBoundRange
      pure . AbsorbingMatched bs $ w8

-- Ranges

byteBoundRange :: Range Int
byteBoundRange = Range.linear 0 64

diffRange :: Int -> Range Int
diffRange diff = let param = abs diff + 1 in
  Range.linear param (param * 2)
