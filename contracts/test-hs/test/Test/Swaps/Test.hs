-- | Tests on the swaps contract.
module Test.Swaps.Test
  ( test_Swap
  ) where

import Prelude hiding (swap)

import Test.Tasty (TestTree, testGroup)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)
import Tezos.Address (unsafeParseAddress)

import Lorentz.Contracts.Swaps
import Lorentz.Test.Consumer
import Lorentz.Value
import Test.Swaps.Util
import Test.Util

test_Swap :: TestTree
test_Swap = testGroup "Basic swap functionality"
  [ simpleHappyPaths
  , statusChecks
  , swapIdChecks
  , authorizationChecks
  , invalidFA2sChecks
  , complexCases
  ]

simpleHappyPaths :: TestTree
simpleHappyPaths = testGroup "Simple happy paths"
  [ nettestScenarioCaps "Simple accepted swap" $ do
      setup <- doSetup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      swap <- originateSwap
      fa2 <- originateFA2 "fa2" setup swap

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: -10
        , (alice, tokenId2) -: 5
        , (bob, tokenId1) -: 10
        , (bob, tokenId2) -: -5
        ] $ do
          withSender (AddressResolved alice) $
            call swap (Call @"Start") SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 10)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId2, 5)]]
              }
          withSender (AddressResolved bob) $
            call swap (Call @"Accept") (SwapId 0)

  , nettestScenarioCaps "Simple cancelled swap" $ do
      setup <- doSetup
      let alice ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
      swap <- originateSwap
      fa2 <- originateFA2 "fa2" setup swap

      assertingBalanceDeltas fa2
        [ (alice, tokenId1) -: 0
        , (alice, tokenId2) -: 0
        ] $ do
          withSender (AddressResolved alice) $
            call swap (Call @"Start") SwapOffer
              { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 10)]]
              , assetsRequested = [mkFA2Assets fa2 [(tokenId2, 5)]]
              }
          withSender (AddressResolved alice) $
            call swap (Call @"Cancel") (SwapId 0)
  ]

statusChecks :: TestTree
statusChecks = testGroup "Statuses"
  [ nettestScenarioCaps "Operations with accepted swap fail" $ do
      swap <- originateSwap

      call swap (Call @"Start") $ SwapOffer [] []
      call swap (Call @"Accept") (SwapId 0)

      call swap (Call @"Accept") (SwapId 0)
        & expectError errSwapFinished

      call swap (Call @"Cancel") (SwapId 0)
        & expectError errSwapFinished

  , nettestScenarioCaps "Operations with cancelled swap fail" $ do
      swap <- originateSwap

      call swap (Call @"Start") $ SwapOffer [] []
      call swap (Call @"Cancel") (SwapId 0)

      call swap (Call @"Accept") (SwapId 0)
        & expectError errSwapCancelled

      call swap (Call @"Cancel") (SwapId 0)
        & expectError errSwapCancelled
  ]

swapIdChecks :: TestTree
swapIdChecks = testGroup "SwapIds"
  [ nettestScenarioCaps "Swap ids are properly assigned and can be worked with" $ do
      setup <- doSetup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      swap <- originateSwap
      fa2 <- originateFA2 "fa2" setup swap

      withSender (AddressResolved alice) $
        for_ [tokenId1, tokenId2, tokenId3] $ \tokenId ->
          call swap (Call @"Start") SwapOffer
            { assetsOffered = []
            , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
            }

      assertingBalanceDeltas fa2
        [ (bob, tokenId1) -: -1
        , (bob, tokenId2) -: -0
        , (bob, tokenId3) -: -1
        ] $ do
          withSender (AddressResolved bob) $ do
            call swap (Call @"Accept") (SwapId 0)
            call swap (Call @"Accept") (SwapId 2)

  , nettestScenarioCaps "Accessing non-existing swap fails respectively" $ do
      swap <- originateSwap

      call swap (Call @"Accept") (SwapId 0)
        & expectError errSwapNotExist
      call swap (Call @"Cancel") (SwapId 0)
        & expectError errSwapNotExist

      call swap (Call @"Start") $ SwapOffer [] []

      call swap (Call @"Accept") (SwapId 1)
        & expectError errSwapNotExist
      call swap (Call @"Cancel") (SwapId 1)
        & expectError errSwapNotExist

      call swap (Call @"Accept") (SwapId 0)
  ]

authorizationChecks :: TestTree
authorizationChecks = testGroup "Authorization checks"
  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
      setup <- doSetup
      let alice ::< bob ::< SNil = sAddresses setup
      let !SNil = sTokens setup
      swap <- originateSwap

      withSender (AddressResolved alice) $
        call swap (Call @"Start") $ SwapOffer [] []

      call swap (Call @"Cancel") (SwapId 0)
        & expectError errNotSwapSeller

      withSender (AddressResolved bob) $
        call swap (Call @"Cancel") (SwapId 0)
        & expectError errNotSwapSeller
  ]

invalidFA2sChecks :: TestTree
invalidFA2sChecks = testGroup "Invalid FA2s"
  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
      setup <- doSetup
      let alice ::< SNil = sAddresses setup
      let tokenId1 ::< SNil = sTokens setup

      fakeFa2 <-
        TAddress . unTAddress <$>
        originateSimple "fake-fa2" ([] :: [Integer]) contractConsumer
      let nonExistingFa2 = TAddress $ unsafeParseAddress "tz1b7p3PPBd3vxmMHZkvtC61C7ttYE6g683F"
      let pseudoFa2s = [("fake FA2", fakeFa2), ("non existing FA2", nonExistingFa2)]

      for_ pseudoFa2s $ \(desc, fa2) -> do
        comment $ "Trying " <> desc
        swap <- originateSwap

        comment "Checking offered FA2"
        withSender (AddressResolved alice) $
          call swap (Call @"Start") SwapOffer
            { assetsOffered = [mkFA2Assets fa2 [(tokenId1, 1)]]
            , assetsRequested = []
            }
            & expectError errSwapOfferedFA2Invalid

        comment "Checking requested FA2"
        withSender (AddressResolved alice) $ do
          call swap (Call @"Start") SwapOffer
            { assetsOffered = []
            , assetsRequested = [mkFA2Assets fa2 [(tokenId1, 1)]]
            }

          call swap (Call @"Accept") (SwapId 0)
            & expectError errSwapRequestedFA2Invalid
  ]

complexCases :: TestTree
complexCases = testGroup "Complex cases"
  [ nettestScenarioCaps "Multiple FA2s" $ do
      setup <- doSetup
      let alice ::< bob ::< SNil = sAddresses setup
      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
      swap <- originateSwap
      fa2_1 <- originateFA2 "fa2-1" setup swap
      fa2_2 <- originateFA2 "fa2-2" setup swap

      assertingBalanceDeltas fa2_1
        [ (alice, tokenId1) -: -100
        , (alice, tokenId2) -: -50
        , (alice, tokenId3) -: 1
        , (bob, tokenId1) -: 100
        , (bob, tokenId2) -: 50
        , (bob, tokenId3) -: -1
        ] $
        assertingBalanceDeltas fa2_2
        [ (alice, tokenId1) -: -1000
        , (bob, tokenId1) -: 1000
        ] $ do
          withSender (AddressResolved alice) $
            call swap (Call @"Start") SwapOffer
              { assetsOffered =
                  [ mkFA2Assets fa2_1
                    [ (tokenId1, 100)
                    , (tokenId2, 50)
                    ]
                  , mkFA2Assets fa2_2
                    [ (tokenId1, 1000)
                    ]
                  ]
              , assetsRequested =
                  [ mkFA2Assets fa2_1
                    [ (tokenId3, 1)
                    ]
                  ]
              }
          withSender (AddressResolved bob) $
            call swap (Call @"Accept") (SwapId 0)

  ]
