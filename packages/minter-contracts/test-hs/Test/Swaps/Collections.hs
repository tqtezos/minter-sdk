module Test.Swaps.Collections where

import Prelude hiding (swap, toStrict)

import qualified Data.Sized as Sized (toList)

import qualified Data.Set as Set

import Hedgehog (Property, property, forAll)

import qualified Lorentz.Contracts.Spec.FA2Interface as FA2I

import qualified Indigo.Contracts.FA2Sample as FA2
import Michelson.Interpret.Pack 

import GHC.Exts (fromList)
import GHC.Integer (negateInteger)

import Test.Tasty (TestTree, testGroup)

import Morley.Nettest
import Morley.Nettest.Tasty (nettestScenarioCaps)

import Lorentz.Address

import qualified Lorentz.Contracts.Swaps.Basic as Basic
import Lorentz.Contracts.Swaps.Collections
import Lorentz.Value
import Lorentz.Test (contractConsumer)

import Lorentz.Contracts.Spec.FA2Interface (TokenId(..))

import Test.Swaps.Basic

import Test.Swaps.Util
import Test.Util
import Test.NonPausableSimpleAdmin

import Tezos.Address (unsafeParseAddress)
import Tezos.Crypto

--type (<=) x y = (x <=? y) ~ 'True
--
--data TestData = TestData
--  { numOffers :: Natural
--  , token1Offer :: Natural
--  , token2Offer :: Natural
--  , request :: [Natural]
--  deriving stock (Show)
--
--genTestData :: Gen TestData
--genTestData = do
--  let genNat = Gen.integral (Range.constant 1 20)
--  numOffers <- genNat
--  token1Offer <- genNat
--  token2Offer <- genNat 
--  request <- 
--  pure $ TestData
--    { numOffers = numOffers
--    , token1Offer = token1Offer
--    , token2Offer = token2Offer
--    , token1Request = token1Request
--    , token2Request = token2Request }

----------------------------------------------------------------------------
-- Permit Tests
----------------------------------------------------------------------------

hprop_Sending_fake_permit_to_offchain_accept_fails :: Property
hprop_Sending_fake_permit_to_offchain_accept_fails =
  property $ do
    clevelandProp $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let adminToken ::< tokenId1 ::< tokenId2 ::< tokenId3 ::< tokenId4 ::< tokenId5 ::< SNil = sTokens setup
      let tokensList = [tokenId1, tokenId2, tokenId3, tokenId4, tokenId5]
      fa2 <- originateFA2 "fa2" setup []
      let fa2Address = toAddress fa2
      swap <- originateOffchainCollections admin fa2Address
      swapId <- (\(Basic.SwapId n) -> n) . 
                nextSwapId . 
                fromVal @CollectionsStorage <$> 
                getStorage' swap 
      withSender admin $
        addCollection (Set.fromList tokensList) swap
      withSender alice $ 
        addOperatorOnTokens tokensList (toAddress swap) alice fa2
      withSender admin $ do
        addOperatorOnTokens [adminToken] (toAddress swap) admin fa2
        call swap (Call @"Start") $ mkSingleOffer SwapOffer
          { assetsOffered = []
          , assetsRequested = [initCollectionId]
          }
      let acceptParam = AcceptParam {
            swapId = Basic.initSwapId , 
            tokensSent = one $ (initCollectionId, tokenId1)
          }
      missignedBytes <- fst <$> mkPermitToForge acceptParam swap
      withSender admin $ do
        (offchainAcceptForged acceptParam alice swap) `expectFailure` failedWith swap
          ([mt|MISSIGNED|], missignedBytes)

hprop_Offchain_accept_not_admin_submitted_fails :: Property
hprop_Offchain_accept_not_admin_submitted_fails =
  property $ do
    clevelandProp $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let adminToken ::< tokenId1 ::< tokenId2 ::< tokenId3 ::< tokenId4 ::< tokenId5 ::< SNil = sTokens setup
      let tokensList = [tokenId1, tokenId2, tokenId3, tokenId4, tokenId5]
      fa2 <- originateFA2 "fa2" setup []
      let fa2Address = toAddress fa2
      swap <- originateOffchainCollections admin fa2Address
      withSender admin $
        addCollection (Set.fromList tokensList) swap
      withSender alice $ 
        addOperatorOnTokens tokensList (toAddress swap) alice fa2
      withSender admin $ do
        addOperatorOnTokens [adminToken] (toAddress swap) admin fa2
        call swap (Call @"Start") $ mkSingleOffer SwapOffer
          { assetsOffered = []
          , assetsRequested = [initCollectionId]
          }
      let tokensSent = one $ (initCollectionId, tokenId1)
      withSender alice $ do
        (offchainAccept tokensSent alice swap) `expectFailure` failedWith swap errNotAdmin

--hprop_Consecutive_offchain_accept_equals_iterative_accept :: Property
--hprop_Consecutive_offchain_accept_equals_iterative_accept =
--    property $ do
--      TestData{numOffers,token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData
--      clevelandProp $ do
--        setup <- doFA2Setup @("addresses" :# 50) @("tokens" :# 2)
--        let admin1 ::< admin2 ::< remainingAddresses = sAddresses setup
--        let addresses = take (fromIntegral numOffers) (Sized.toList remainingAddresses) 
--        let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
--        swap1 <- originateOffchainCollections admin1
--        swap2 <- originateOffchainCollections admin2
--        fa2_1 <- originateFA2 "fa2_1" setup [swap1]
--        fa2_2 <- originateFA2 "fa2_2" setup [swap2]
--        withSender admin1 $ do 
--          call swap1 (Call @"Start") $ mkNOffers numOffers SwapOffer
--               { assetsOffered = Basic.tokens  $ mkFA2Assets fa2_1 [(tokenId1, token1Offer), (tokenId2, token2Offer)]
--               , assetsRequested = [mkFA2Assets fa2_1 [(tokenId1, token1Request), (tokenId2, token2Request)]]
--               }
--        withSender admin2 $ do 
--          call swap2 (Call @"Start") $ mkNOffers numOffers SwapOffer
--               { assetsOffered = Basic.tokens  $ mkFA2Assets fa2_2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]
--               , assetsRequested = [mkFA2Assets fa2_2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
--               }
--        withSender admin1 $ do
--          offchainAcceptAllConsecutive addresses swap1
--        withSender admin2 $ do
--          offchainAcceptBatch addresses swap2
--      
--        swapStorage1 <-  toVal <$>
--                         burnSwapStorage <$>
--                         fromVal @CollectionsStorage <$> 
--                         getStorage' swap1
--        swapStorage2 <-  toVal <$>
--                         burnSwapStorage <$>
--                         fromVal @CollectionsStorage <$> 
--                         getStorage' swap2
--        swapStorage1 @== swapStorage2
--
------------------------------------------------------------------------------
---- Swap + Burn Tests Using Offchain_accept
------------------------------------------------------------------------------

hprop_Accepting_with_zero_balance_fails :: Property
hprop_Accepting_with_zero_balance_fails =
    property $ do
      clevelandProp $ do
          setup <- doFA2Setup
          let admin ::< SNil = sAddresses setup
          let adminToken ::< tokenId1 ::< tokenId2 ::< SNil = sTokens setup
          let tokensList = [tokenId1, tokenId2]
          fa2 <- originateFA2 "fa2" setup []
          let fa2Address = toAddress fa2
          swap <- originateOffchainCollections admin fa2Address
          addressWithZeroBalance <- newAddress "test"
          withSender admin $
            addCollection (Set.fromList tokensList) swap
          withSender addressWithZeroBalance $ 
            addOperatorOnTokens tokensList (toAddress swap) addressWithZeroBalance fa2
          withSender admin $ do
            addOperatorOnTokens [adminToken] (toAddress swap) admin fa2
          withSender admin $
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered = Basic.tokens $ mkFA2Assets fa2 [(adminToken, 10)]
              , assetsRequested = [initCollectionId, initCollectionId]
              }
          let tokensSent = Set.fromList [(initCollectionId, tokenId1), (initCollectionId, tokenId2)]
          withSender admin
            ((offchainAccept tokensSent addressWithZeroBalance swap)
              `expectFailure` failedWith fa2 (errSwapRequestedFA2BalanceInvalid 1 0))  

--hprop_Start_callable_by_admin_only :: Property
--hprop_Start_callable_by_admin_only = 
--  property $ do
--   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
--   clevelandProp $ do
--     setup <- doFA2Setup
--     let admin ::< nonAdmin ::< SNil = sAddresses setup
--     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
--     swap <- originateOffchainCollections admin
--     fa2 <- originateFA2 "fa2" setup [swap]
--     withSender nonAdmin 
--       (call swap (Call @"Start") (mkNOffers numOffers SwapOffer
--         { assetsOffered = Basic.tokens  $ mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]
--         , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
--         }) & expectError swap errNotAdmin)
--
--hprop_Correct_final_balances_on_acceptance :: Property
--hprop_Correct_final_balances_on_acceptance = 
--  property $ do
--   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
--   clevelandProp  $ do
--      setup <- doFA2Setup
--      let admin ::< alice ::< SNil = sAddresses setup
--      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
--      swap <- originateOffchainCollections admin
--      fa2 <- originateFA2 "fa2" setup [swap]
--      
--      assertingBurnAddressUnchanged swap $ do 
--        assertingBalanceDeltas fa2
--          [ (admin, tokenId1) -: negateInteger (fromIntegral $ token1Offer * numOffers)
--          , (admin, tokenId2) -: negateInteger (fromIntegral $ token2Offer * numOffers)
--          , (nullAddress, tokenId1) -: (fromIntegral $ token1Request)
--          , (nullAddress, tokenId2) -: (fromIntegral $ token2Request)
--          , (alice, tokenId1) -: fromIntegral token1Offer - fromIntegral token1Request
--          , (alice, tokenId2) -: fromIntegral token2Offer - fromIntegral token2Request
--          ] $ do
--            withSender admin $
--              call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
--                { assetsOffered = Basic.tokens  $ mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]
--                , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
--                }
--            withSender admin $
--              offchainAccept alice swap
--  
--hprop_Correct_final_balances_on_cancel :: Property
--hprop_Correct_final_balances_on_cancel = 
--  property $ do
--   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
--   clevelandProp  $ do
--      setup <- doFA2Setup
--      let admin ::< alice ::< SNil = sAddresses setup
--      let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
--      swap <- originateOffchainCollections admin
--      fa2 <- originateFA2 "fa2" setup [swap]
--
--      assertingBurnAddressUnchanged swap $ do 
--        assertingBalanceDeltas fa2
--          [ (admin, tokenId1) -: 0
--          , (admin, tokenId2) -: 0
--          , (alice, tokenId1) -: 0
--          , (alice, tokenId2) -: 0
--          , (nullAddress, tokenId1) -: 0
--          , (nullAddress, tokenId2) -: 0
--          ] $ do
--            withSender admin $
--              call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
--                { assetsOffered = Basic.tokens  $ mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]
--                , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
--                }
--            withSender admin $
--              call swap (Call @"Cancel") Basic.initSwapId
--  
--hprop_Correct_num_tokens_transferred_to_contract_on_start :: Property
--hprop_Correct_num_tokens_transferred_to_contract_on_start = 
--  property $ do
--   TestData{numOffers, token1Offer, token2Offer} <- forAll genTestData 
--   clevelandProp  $ do
--     setup <- doFA2Setup
--     let admin ::< SNil = sAddresses setup
--     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
--     swap <- originateOffchainCollections admin
--     fa2 <- originateFA2 "fa2" setup [swap]
--
--     assertingBurnAddressUnchanged swap $ do 
--       assertingBalanceDeltas fa2
--         [ (admin, tokenId1) -: negateInteger (fromIntegral $ token1Offer * numOffers)
--         , (admin, tokenId2) -: negateInteger (fromIntegral $ token2Offer * numOffers)
--         ] $ do
--           withSender admin $
--             call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
--               { assetsOffered = Basic.tokens  $ mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]
--               , assetsRequested = [mkFA2Assets fa2 [(tokenId2, 1)]]
--               }
--
--hprop_Contract_balance_goes_to_zero_when_sale_concludes :: Property
--hprop_Contract_balance_goes_to_zero_when_sale_concludes = 
--  property $ do
--   TestData{numOffers, token1Offer, token2Offer, token1Request, token2Request} <- forAll genTestData 
--   clevelandProp  $ do
--     setup <- doFA2Setup
--     let admin ::< alice ::< SNil = sAddresses setup
--     let tokenId1 ::< tokenId2 ::< SNil = sTokens setup
--     swap <- originateOffchainCollections admin
--     let swapAddress = toAddress swap
--     fa2 <- originateFA2 "fa2" setup [swap]
--     assertingBurnAddressUnchanged swap $ do 
--
--       assertingBalanceDeltas fa2
--         [ (swapAddress, tokenId1) -: 0
--         , (swapAddress, tokenId2) -: 0
--         ] $ do
--           withSender admin $
--             call swap (Call @"Start") $ mkNOffers numOffers SwapOffer
--               { assetsOffered = Basic.tokens  $ mkFA2Assets fa2 [(tokenId1, token1Offer), (tokenId2, token2Offer)]
--               , assetsRequested = [mkFA2Assets fa2 [(tokenId1, token1Request), (tokenId2, token2Request)]]
--               }
--           withSender admin $
--              replicateM_ (fromIntegral numOffers) $ do
--                offchainAccept alice swap 
--
--statusChecks :: TestTree
--statusChecks = testGroup "Statuses"
--  [ nettestScenarioCaps "Operations with accepted swap fail" $ do
--      (swap, admin) <- originateOffchainSwapBurnFeeWithAdmin
--      setup <- doFA2Setup
--      let alice ::< SNil = sAddresses setup
--      let !SNil = sTokens setup
--      withSender admin $ do
--        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] ([], 0))
--        offchainAccept' alice swap
--      
--      withSender admin $
--        offchainAccept' alice swap
--        & expectError swap errSwapFinished
--      
--      withSender admin $
--        offchainAccept' alice swap
--        & expectError swap errSwapFinished
--
--  , nettestScenarioCaps "Operations with cancelled swap fail" $ do
--      (swap, admin) <- originateOffchainSwapBurnFeeWithAdmin
--      setup <- doFA2Setup
--      let alice ::< SNil = sAddresses setup
--      let !SNil = sTokens setup
--      withSender admin $
--        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] ([], 0))
--      withSender admin $
--        call swap (Call @"Cancel") Basic.initSwapId
--
--      withSender admin $
--        offchainAccept' alice swap
--        & expectError swap errSwapCancelled
--      
--      withSender admin $
--        call swap (Call @"Cancel") Basic.initSwapId
--          & expectError swap errSwapCancelled
--  ]
--
--swapIdChecks :: TestTree
--swapIdChecks = testGroup "SwapIds"
--  [ nettestScenarioCaps "Swap ids are properly assigned and can be worked with" $ do
--      setup <- doFA2Setup
--      let admin ::< alice ::< SNil = sAddresses setup
--      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
--      swap <- originateOffchainCollections admin
--      fa2 <- originateFA2 "fa2" setup [swap]
--
--      withSender admin $
--        for_ [tokenId1, tokenId2, tokenId3] $ \tokenId ->
--          call swap (Call @"Start") $ mkSingleOffer SwapOffer
--            { assetsOffered = []
--            , assetsRequested = [mkFA2Assets fa2 [(tokenId, 1)]]
--            }
--
--      assertingBalanceDeltas fa2
--        [ (alice, tokenId1) -: -1
--        , (alice, tokenId2) -:  0
--        , (alice, tokenId3) -: -1
--        ] $ do
--          withSender alice $ do
--            (\swapId -> offchainAcceptSwapId' swapId alice swap) (getSwapId Basic.initSwapId)
--            (\swapId -> offchainAcceptSwapId' swapId alice swap) (getSwapId $ incrementSwapId $ incrementSwapId Basic.initSwapId)
--
--  , nettestScenarioCaps "Accessing non-existing swap fails respectively" $ do
--      (swap, admin) <- originateOffchainSwapBurnFeeWithAdmin
--      setup <- doFA2Setup
--      let alice ::< SNil = sAddresses setup
--      let !SNil = sTokens setup
--      withSender admin $
--        offchainAccept' alice swap
--          & expectError swap errSwapNotExist
--      
--      withSender admin 
--        (call swap (Call @"Cancel") Basic.initSwapId
--          & expectError swap errSwapNotExist)
--      
--      withSender admin $ do 
--        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] ([], 0))
--
--        (\swapId -> offchainAcceptSwapId' swapId alice swap) (getSwapId $ incrementSwapId Basic.initSwapId)
--          & expectError swap errSwapNotExist
--
--        call swap (Call @"Cancel") Basic.initSwapId
--          & expectError swap errSwapNotExist
--
--        (\swapId -> offchainAcceptSwapId' swapId alice swap) (getSwapId Basic.initSwapId)
--  ]
--
--
--authorizationChecks :: TestTree
--authorizationChecks = testGroup "Authorization checks"
--  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
--      setup <- doFA2Setup
--      let admin ::< alice ::< SNil = sAddresses setup
--      let !SNil = sTokens setup
--      swap <- originateOffchainCollections admin
--
--      withSender admin $
--        call swap (Call @"Start") $ mkSingleOffer (SwapOffer [] ([], 0))
--
--      call swap (Call @"Cancel") Basic.initSwapId
--        & expectError swap errNotSwapSeller
--
--      withSender alice $
--        call swap (Call @"Cancel") Basic.initSwapId
--        & expectError swap errNotSwapSeller
--  ]
--
--invalidFA2sChecks :: TestTree
--invalidFA2sChecks = testGroup "Invalid FA2s"
--  [ nettestScenarioCaps "Swap can be cancelled by seller only" $ do
--      setup <- doFA2Setup
--      let admin ::< alice ::< SNil = sAddresses setup
--      let tokenId1 ::< SNil = sTokens setup
--
--      fakeFa2 <-
--        TAddress . unTAddress <$>
--        originateSimple "fake-fa2" ([] :: [Integer]) contractConsumer
--      let nonExistingFa2 = TAddress $ unsafeParseAddress "tz1b7p3PPBd3vxmMHZkvtC61C7ttYE6g683F"
--      let pseudoFa2s = [("fake FA2", fakeFa2), ("non existing FA2", nonExistingFa2)]
--
--      for_ pseudoFa2s $ \(desc, fa2) -> do
--        comment $ "Trying " <> desc
--        swap <- originateOffchainCollections admin
--
--        withSender admin $
--          call swap (Call @"Update_allowed") (mkAllowlistSimpleParam [fa2])
--
--        comment "Checking offered FA2"
--        withSender admin $
--          call swap (Call @"Start") (mkSingleOffer SwapOffer
--            { assetsOffered = Basic.tokens  $ mkFA2Assets fa2 [(tokenId1, 1)]
--            , assetsRequested = []
--            })
--            & expectError swap errSwapOfferedFA2Invalid
--
--        comment "Checking requested FA2"
--        withSender admin $ do
--          call swap (Call @"Start") $ mkSingleOffer SwapOffer
--            { assetsOffered = []
--            , assetsRequested = [mkFA2Assets fa2 [(tokenId1, 1)]]
--            }
--
--          withSender admin
--            (offchainAccept' alice swap)
--            & expectError swap errSwapRequestedFA2Invalid
--  ]
--
--complexCases :: TestTree
--complexCases = testGroup "Complex cases"
--  [ nettestScenarioCaps "Multiple FA2s" $ do
--      setup <- doFA2Setup
--      let admin ::< alice ::< SNil = sAddresses setup
--      let tokenId1 ::< tokenId2 ::< tokenId3 ::< SNil = sTokens setup
--      swap <- originateOffchainCollections admin
--      fa2_1 <- originateFA2 "fa2-1" setup [swap]
--      fa2_2 <- originateFA2 "fa2-2" setup [swap]
--
--      assertingBalanceDeltas fa2_1
--        [ (admin, tokenId1) -: -100
--        , (admin, tokenId2) -: -50
--        , (nullAddress , tokenId3) -: 1
--        , (alice, tokenId1) -: 100
--        , (alice, tokenId2) -: 50
--        , (alice, tokenId3) -: -1
--        ] $
--        assertingBalanceDeltas fa2_2
--        [ (admin, tokenId1) -: -1000
--        , (alice, tokenId1) -: 1000
--        ] $ do
--          withSender admin $
--            call swap (Call @"Start") $ mkSingleOffer SwapOffer
--              { assetsOffered =
--                  Basic.tokens  $ mkFA2Assets fa2_1
--                    [ (tokenId1, 100)
--                    , (tokenId2, 50)
--                    ]
--                  , mkFA2Assets fa2_2
--                    [ (tokenId1, 1000)
--                    ]
--                  
--              , assetsRequested =
--                  [ mkFA2Assets fa2_1
--                    [ (tokenId3, 1)
--                    ]
--                  ]
--              }
--          withSender admin $
--            offchainAccept' alice swap
--
--  ]

----------------------------------------------------------------------------
-- Swap + Burn Tests Using normal Accept
----------------------------------------------------------------------------
test_Integrational :: TestTree
test_Integrational = testGroup "Integrational"
  [ -- Check that storage updates work
    nettestScenarioCaps "Simple accepted swap" $ do
      setup <- doFA2Setup
      let admin ::< alice ::< SNil = sAddresses setup
      let adminToken ::< tokenId1 ::< tokenId2 ::< tokenId3 ::< tokenId4 ::< tokenId5 ::< SNil = sTokens setup
      fa2 <- originateFA2 "fa2" setup []
      let fa2Address = toAddress fa2
      swap <- originateOffchainCollections admin fa2Address
      withSender alice $ 
        addOperatorOnTokens' [tokenId1, tokenId2, tokenId3, tokenId4, tokenId5] (toAddress swap) alice fa2
      withSender admin $ 
        addOperatorOnTokens' [adminToken] (toAddress swap) admin fa2

      assertingBalanceDeltas fa2
        [ (admin, adminToken) -: -10
        , (admin, tokenId1) -: 0
        , (admin, tokenId2) -: 0
        , (nullAddress, tokenId1) -: 1
        , (nullAddress, tokenId2) -: 1
        , (nullAddress, adminToken) -: 0
        , (alice, tokenId1) -: -1
        , (alice, tokenId2) -: -1
        , (alice, adminToken) -: 10
        ] $ do
          withSender admin $ do  
            addCollection' (Set.fromList [tokenId1, tokenId2, tokenId3, tokenId4, tokenId5]) swap
            addCollection' (Set.fromList [tokenId1, tokenId4, tokenId5]) swap
            call swap (Call @"Start") $ mkSingleOffer SwapOffer
              { assetsOffered = Basic.tokens  $ mkFA2Assets fa2 [(adminToken, 10)]
              , assetsRequested = [initCollectionId, initCollectionId, incrementCollectionId initCollectionId]
              }
          withSender alice $
            call swap (Call @"Accept") AcceptParam 
              {
                swapId = Basic.initSwapId , 
                tokensSent = Set.fromList [(initCollectionId, tokenId1), (initCollectionId, tokenId2), (incrementCollectionId initCollectionId, tokenId5)] 
              }
  ]

----------------------------------------------------------------------------
-- Admin Checks 
----------------------------------------------------------------------------


test_AdminChecks :: TestTree
test_AdminChecks = do  
  adminOwnershipTransferChecks (\admin -> originateOffchainCollections admin exampleFA2Address)

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------
addCollection' :: (HasCallStack, MonadNettest caps base m) => Set TokenId -> TAddress OffchainCollectionsEntrypoints -> m ()
addCollection' collection contract = call contract (Call @"Add_collection") collection

addCollections' :: (HasCallStack, MonadEmulated caps base m) => [TokenId] -> TAddress OffchainCollectionsEntrypoints -> m [[TokenId]]
addCollections' tokenIds contract = do
  let collections =  map sort (filter (not . null) (filterM (const [True, False]) tokenIds))
  mapM_ (\collection -> addCollection (Set.fromList collection) contract) collections 
  return collections

addCollection :: (HasCallStack, MonadEmulated caps base m) => Set TokenId -> TAddress OffchainCollectionsEntrypoints -> m ()
addCollection collection contract = call contract (Call @"Add_collection") collection

addCollections :: (HasCallStack, MonadEmulated caps base m) => [TokenId] -> TAddress OffchainCollectionsEntrypoints -> m [[TokenId]]
addCollections tokenIds contract = do 
  let collections = map sort (filter (not . null) (filterM (const [True, False]) tokenIds))
  mapM_ (\collection -> addCollection (Set.fromList collection) contract) collections 
  return collections 

addOperatorOnTokens :: (HasCallStack, MonadEmulated caps base m) => [TokenId] -> Address -> Address -> TAddress FA2.FA2SampleParameter -> m() 
addOperatorOnTokens tokens operator owner fa2 = 
  call fa2 (Call @"Update_operators") operatorParamList
  where 
    operatorParamList = map (\token -> FA2I.AddOperator FA2I.OperatorParam
              { opOwner = owner
              , opOperator = toAddress operator
              , opTokenId = token
              }) tokens

addOperatorOnTokens' :: (HasCallStack, MonadNettest caps base m) => [TokenId] -> Address -> Address -> TAddress FA2.FA2SampleParameter  -> m() 
addOperatorOnTokens' tokens operator owner fa2 = 
  call fa2 (Call @"Update_operators") operatorParamList
  where 
    operatorParamList = map (\token -> FA2I.AddOperator FA2I.OperatorParam
              { opOwner = owner
              , opOperator = toAddress operator
              , opTokenId = token
              }) tokens

-- N addresses accept all N assets in a sale conseutively, and then all N are confirmed
--offchainAcceptAllConsecutive :: (HasCallStack, MonadEmulated caps base m) => [Address] -> TAddress PermitSwapBurnFeeEntrypoints -> m ()
--offchainAcceptAllConsecutive addresses contract = do
--  forM_ addresses $ \buyer -> do
--      offchainAccept buyer contract
--
--offchainAcceptBatch :: (HasCallStack, MonadEmulated caps base m) => [Address] -> TAddress PermitSwapBurnFeeEntrypoints -> m ()
--offchainAcceptBatch buyers contract = do
--  param <- forM buyers $ \buyer -> do
--    buyerPK <- getPublicKey buyer
--    unsigned <- mkPermitToSign swapId contract
--    signature <- signBytes unsigned buyer
--    return OffchainAcceptParam { 
--        swapId = swapId
--      , permit = Permit
--          {
--            signerKey = buyerPK
--          , signature = signature
--          } 
--      } 
--  call contract (Call @"Offchain_accept") (toList param) 
--  where swapId = 1
--
mkPermitToForge :: (HasCallStack, MonadEmulated caps base m) => AcceptParam -> TAddress OffchainCollectionsEntrypoints -> m (ByteString, PublicKey)
mkPermitToForge acceptParam contract = do 
  aliasAddress <- newAddress "forged"
  aliasPK <- getPublicKey aliasAddress
  unsignedPermit <- mkPermitToSign acceptParam contract 
  pure (unsignedPermit, aliasPK)

mkPermitToSign :: (HasCallStack, MonadEmulated caps base m) => AcceptParam -> TAddress OffchainCollectionsEntrypoints -> m ByteString
mkPermitToSign acceptParam contract = do 
  marketplaceChainId <- getChainId
  let unsigned = packValue' $ toVal ((marketplaceChainId, contractAddress), (0 :: Natural, acceptParamHash))
  pure unsigned
  where acceptParamHash = blake2b $ packValue' $ toVal acceptParam
        contractAddress = toAddress contract

offchainAcceptSwapId :: (HasCallStack, MonadEmulated caps base m) => AcceptParam -> Address -> TAddress OffchainCollectionsEntrypoints -> m ()
offchainAcceptSwapId acceptParam buyer contract = do
  buyerPK <- getPublicKey buyer
  unsigned <- mkPermitToSign acceptParam contract
  signature <- signBytes unsigned buyer 
  call contract (Call @"Offchain_accept") 
    [OffchainAcceptParam
      {
        acceptParam = acceptParam
      , permit = Permit
          {
            signerKey = buyerPK
          , signature = signature
          } 
      }
    ]


offchainAccept :: (HasCallStack, MonadEmulated caps base m) => Set (CollectionId, TokenId) -> Address -> TAddress OffchainCollectionsEntrypoints -> m ()
offchainAccept tokensSent = offchainAcceptSwapId AcceptParam {
    swapId = Basic.initSwapId, 
    tokensSent = tokensSent
  }
--
--mkPermitToSign' :: (HasCallStack, MonadNettest caps base m) => Natural -> TAddress PermitSwapBurnFeeEntrypoints -> m ByteString
--mkPermitToSign' swapId contract = do 
--  marketplaceChainId <- getChainId
--  let unsigned = packValue' $ toVal ((marketplaceChainId, contractAddress), (0 :: Natural, swapIdHash))
--  pure unsigned
--  where swapIdHash = blake2b $ packValue' $ toVal swapId 
--        contractAddress = toAddress contract
--
--offchainAcceptSwapId' :: (HasCallStack, MonadNettest caps base m) => Natural -> Address -> TAddress PermitSwapBurnFeeEntrypoints -> m ()
--offchainAcceptSwapId' swapId buyer contract = do
--  buyerPK <- getPublicKey buyer
--  unsigned <- mkPermitToSign' swapId contract
--  signature <- signBytes unsigned buyer 
--  call contract (Call @"Offchain_accept") 
--    [OffchainAcceptParam
--      {
--        swapId = swapId
--      , permit = Permit
--          {
--            signerKey = buyerPK
--          , signature = signature
--          } 
--      }
--    ]
--
--offchainAccept' :: (HasCallStack, MonadNettest caps base m) => Address -> TAddress PermitSwapBurnFeeEntrypoints -> m ()
--offchainAccept' = offchainAcceptSwapId' 1
--
--
offchainAcceptForged :: (HasCallStack, MonadEmulated caps base m) => AcceptParam -> Address -> TAddress OffchainCollectionsEntrypoints -> m ByteString
offchainAcceptForged acceptParam buyer contract = do
  (unsigned, forgedPK) <- mkPermitToForge acceptParam contract
  signature <- signBytes unsigned buyer 
  (\() -> unsigned) <$> call contract (Call @"Offchain_accept") 
    [OffchainAcceptParam
      {
        acceptParam = acceptParam
      , permit = Permit
          {
            signerKey = forgedPK
          , signature = signature
          } 
      }
    ] 
--
--assertingBurnAddressStatus
--  :: (MonadEmulated caps base m, HasCallStack)
--  => TAddress b
--  -> m a
--  -> (Address -> Address -> m ())
--  -> m a
--assertingBurnAddressStatus swapContract action changedStatus = do
--  initBurnAddress <- getBurnAddress swapContract
--  res <- action
--  finalBurnAddress <- getBurnAddress swapContract
--  initBurnAddress `changedStatus` finalBurnAddress
--  return res
--    where
--      getBurnAddress c = do 
--        storage <- fromVal @CollectionsStorage <$> getStorage' c
--        pure $ (burnAddress . burnSwapStorage) storage
--
--assertingBurnAddressUnchanged 
--  :: (MonadEmulated caps base m, HasCallStack)
--  => TAddress b
--  -> m a
--  -> m a 
--assertingBurnAddressUnchanged swapContract action = 
--   assertingBurnAddressStatus swapContract action (@==)
--
--assertingBurnAddressChanged
--  :: (MonadEmulated caps base m, HasCallStack)
--  => TAddress b
--  -> m a
--  -> m a 
--assertingBurnAddressChanged swapContract action = 
--   assertingBurnAddressStatus swapContract action (@/=)
