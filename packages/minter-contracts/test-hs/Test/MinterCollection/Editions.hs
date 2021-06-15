module Test.MinterCollection.Editions where

import qualified Data.Map as Map
import Hedgehog (Gen, Property, forAll, property)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Gen.Michelson as Gen
import qualified Hedgehog.Range as Range
import Morley.Nettest

import Lorentz
import Lorentz.Test (contractConsumer)
import Michelson.Typed (convertContract, untypeValue)
import qualified Unsafe

import Lorentz.Contracts.MinterCollection.Editions
import qualified Lorentz.Contracts.MinterCollection.Nft.Asset as NftAsset
import qualified Lorentz.Contracts.MinterCollection.Nft.Token as NftToken
import qualified Lorentz.Contracts.PausableAdminOption as Admin
import Lorentz.Contracts.Spec.FA2Interface (TokenId(..), mkFA2View)
import qualified Lorentz.Contracts.Spec.FA2Interface as FA2
import Test.Util (clevelandProp)

hprop_MintEditions_creates_one_entry_for_each_param :: Property
hprop_MintEditions_creates_one_entry_for_each_param =
  property $ do
    editionRuns <- forAll genEditionRunDataList
    clevelandProp $ do
      (initialStorage, admin) <- mkStorage $ adjustMaxEditionsPerRun editionRuns
      editions <- originateEditionsContract initialStorage

      withSender admin $ do
        mintEditionRuns editions editionRuns

        let expectedEditionsMetadata =
              BigMap . Map.fromList $ editionRuns <&> \editionRun ->
                ( dataEditionId editionRun
                , EditionMetadata
                    { creator = admin
                    , editionInfo = dataEditionInfo editionRun
                    , numberOfEditions = dataNumberOfEditions editionRun
                    , numberOfEditionsToDistribute = dataNumberOfEditions editionRun
                    }
                )

        st <- fromVal @Storage <$> getStorage' editions
        editionsMetadata st @== expectedEditionsMetadata

hprop_NextEditionId_is_incremented_by_the_number_of_edition_runs_minted :: Property
hprop_NextEditionId_is_incremented_by_the_number_of_edition_runs_minted =
  property $ do
    editionRuns <- forAll genEditionRunDataList
    clevelandProp $ do
      (initialStorage, admin) <- mkStorage $ adjustMaxEditionsPerRun editionRuns
      editions <- originateEditionsContract initialStorage

      withSender admin $ do
        mintEditionRuns editions editionRuns

        st <- fromVal @Storage <$> getStorage' editions
        nextEditionId st @== genericLength editionRuns

hprop_Each_distribution_mints_a_new_TokenId :: Property
hprop_Each_distribution_mints_a_new_TokenId =
  property $ do
    editionRuns <- forAll genEditionRunDataList
    clevelandProp $ do
      (initialStorage, admin) <- mkStorage $ adjustMaxEditionsPerRun editionRuns
      editions <- originateEditionsContract initialStorage
      receivers <- forM editionRuns createReceivers
      consumer <- originateSimple "balance-response-consumer" [] (contractConsumer @[FA2.BalanceResponseItem])

      withSender admin $ do
        mintEditionRuns editions editionRuns

        forM_ (editionRuns `zip` receivers) \(editionRun, rcvs) ->
          forM_ rcvs \rcv -> do
            -- Check how many editions are still available.
            st <- fromVal @Storage <$> getStorage' editions
            let toDistribute =
                  st
                    & getEditionMetadata (dataEditionId editionRun)
                    <&> numberOfEditionsToDistribute
                    & Unsafe.fromJust

            distributeEditions editions [editionRun] [[rcv]]

            -- According to the spec:
            -- > Each distribution mints a new token to a token_id equal to
            -- > edition_id * max_editions_per_run + (number_of_editions - number_of_editions_to_distribute)
            let expectedTokenId =
                  unEditionId (dataEditionId editionRun)
                  * maxEditionsPerRun st
                  + (dataNumberOfEditions editionRun - toDistribute)

            -- Check that a new FA2 Token ID was minted.
            let balanceRequest = FA2.BalanceRequestItem rcv (TokenId expectedTokenId)
            call editions (Call @"Balance_of") (mkFA2View [balanceRequest] consumer)
            consumerStorage <- fromVal @[[FA2.BalanceResponseItem]] <$> getStorage consumer
            safeHead consumerStorage @== Just [FA2.BalanceResponseItem balanceRequest 1]

hprop_Distributing_n_editions_decrements_NumberOfEditionsToDistribute_by_n :: Property
hprop_Distributing_n_editions_decrements_NumberOfEditionsToDistribute_by_n =
  property $ do
    editionRuns <- forAll genEditionRunDataList
    clevelandProp $ do
      (initialStorage, admin) <- mkStorage $ adjustMaxEditionsPerRun editionRuns
      editions <- originateEditionsContract initialStorage
      receivers <- forM editionRuns createReceivers

      withSender admin $ do
        mintEditionRuns editions editionRuns
        distributeEditions editions editionRuns receivers

        st <- fromVal @Storage <$> getStorage' editions
        forM_ editionRuns \er ->
          (st & getEditionMetadata (dataEditionId er) <&> numberOfEditionsToDistribute)
            @== Just (dataNumberOfEditions er - dataNumberToDistribute er)

hprop_Distributing_more_editions_than_are_available_fails :: Property
hprop_Distributing_more_editions_than_are_available_fails =
  property $ do
    editionRuns <- forAll $ do
      editionRunCount <- Gen.int (Range.linear 0 10)
      -- Edition IDs are sequential, starting at 0
      forM [0.. editionRunCount - 1] \editionId -> do
        numberOfEditions <- Gen.integral (Range.linear 0 30)
        toDistribute <- Gen.integral (Range.linear (numberOfEditions + 1) 100)
        pure $ EditionRunData (fromIntegral editionId) mempty numberOfEditions toDistribute

    clevelandProp $ do
      (initialStorage, admin) <- mkStorage $ adjustMaxEditionsPerRun editionRuns
      editions <- originateEditionsContract initialStorage
      receivers <- forM editionRuns createReceivers

      withSender admin $ do
        mintEditionRuns editions editionRuns

        forM_ (editionRuns `zip` receivers) \(editionRun, rcvs) ->
          distributeEditions editions [editionRun] [rcvs]
            `expectFailure` failedWith editions [mt|NO_EDITIONS_TO_DISTRIBUTE|]

hprop_Calling_MintEditions_many_times_is_the_same_as_calling_it_once :: Property
hprop_Calling_MintEditions_many_times_is_the_same_as_calling_it_once =
  property $ do
    editionRuns <- forAll genEditionRunDataList
    clevelandProp $ do
      (initialStorage, admin) <- mkStorage $ adjustMaxEditionsPerRun editionRuns

      -- Create two contracts, with the same initial storage. Then:
      --   * Using the first contract, mint all editions at once, in one call
      --   * Using the second contract, mint all editions, one at a time
      -- Then check that their storages are equal.
      editions1 <- originateEditionsContract initialStorage
      editions2 <- originateEditionsContract initialStorage

      withSender admin $ do
        -- Mint all editions in one single call
        mintEditionRuns editions1 editionRuns

        -- Mint all editions, one by one
        forM_ editionRuns \editionRun ->
          mintEditionRuns editions2 [editionRun]

        st1 <- fromVal @Storage <$> getStorage' editions1
        st2 <- fromVal @Storage <$> getStorage' editions2

        st1 @== st2

hprop_Calling_DistributeEditions_many_times_is_the_same_as_calling_it_once :: Property
hprop_Calling_DistributeEditions_many_times_is_the_same_as_calling_it_once =
  property $ do
    editionRuns <- forAll genEditionRunDataList
    clevelandProp $ do
      (initialStorage, admin) <- mkStorage $ adjustMaxEditionsPerRun editionRuns
      receivers <- forM editionRuns createReceivers

      -- Create two contracts, with the same initial storage. Then:
      --   * Using the first contract, distribute all editions at once, in one call
      --   * Using the second contract, distribute all editions, one at a time
      -- Then check that their storages are equal.
      editions1 <- originateEditionsContract initialStorage
      editions2 <- originateEditionsContract initialStorage

      withSender admin $ do
        mintEditionRuns editions1 editionRuns
        mintEditionRuns editions2 editionRuns

        -- Distribute all editions in one single call
        distributeEditions editions1 editionRuns receivers

        -- Distribute all editions, one by one
        forM_ (editionRuns `zip` receivers) \(editionRun, rcvs) ->
          forM rcvs \rcv ->
            distributeEditions editions2 [editionRun] [[rcv]]

        st1 <- fromVal @Storage <$> getStorage' editions1
        st2 <- fromVal @Storage <$> getStorage' editions2

        st1 @== st2

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

originateEditionsContract :: MonadNettest caps base m => Storage -> m (TAddress Entrypoints)
originateEditionsContract storage =
  originateSimple "editions" storage editionsContract

mintEditionRuns :: (HasCallStack, MonadNettest caps base m) => TAddress Entrypoints -> [EditionRunData] -> m ()
mintEditionRuns addr editionRuns =
  call addr (Call @"Mint_editions") $
    editionRuns <&> \editionRun ->
      MintEditionParam
        { editionInfo = dataEditionInfo editionRun
        , numberOfEditions = dataNumberOfEditions editionRun
        }

distributeEditions :: (HasCallStack, MonadNettest caps base m) => TAddress Entrypoints -> [EditionRunData] -> [[Address]] -> m ()
distributeEditions addr editionRuns receivers =
  call addr (Call @"Distribute_editions") $
    (editionRuns `zip` receivers) <&> \(editionRun, rcvs) ->
      DistributeEditionParam
        { editionId = dataEditionId editionRun
        , receivers = rcvs
        }

-- | Generate a list of accounts to distribute editions to.
createReceivers :: MonadNettest caps base m => EditionRunData -> m [Address]
createReceivers editionRun = do
  forM [1 .. dataNumberToDistribute editionRun] \i ->
    newAddress ("editions-receiver-" <> show i)

-- | Create the contract's storage and an admin account.
mkStorage :: MonadNettest caps base m => (Storage -> Storage) -> m (Storage, Address)
mkStorage modifyStorage = do
  admin <- newAddress "editions-admin"
  let defaultStorage =
        Storage
          { nextEditionId = 0
          , maxEditionsPerRun = 0
          , editionsMetadata = mempty
          , nftAssetStorage = NftAsset.Storage
              { assets = NftToken.Storage
                  { ledger = mempty
                  , operators = mempty
                  }
              , admin = Admin.AdminStorage
                  { admin = admin
                  , pendingAdmin = Nothing
                  , paused = False
                  }
              , metadata = mempty
              }
          }
  pure (modifyStorage defaultStorage, admin)

-- | Given a list of edition runs to be minted,
-- sets the storage's `max_editions_per_run` to the correct limit.
adjustMaxEditionsPerRun :: [EditionRunData] -> Storage -> Storage
adjustMaxEditionsPerRun editionRuns st =
  st
    { maxEditionsPerRun =
        if null editionRuns
          then 0
          else maximum (dataNumberOfEditions <$> editionRuns)
    }

getEditionMetadata :: EditionId -> Storage -> Maybe EditionMetadata
getEditionMetadata editionId st =
  st
    & editionsMetadata
    & unBigMap
    & Map.lookup editionId

----------------------------------------------------------------------------
-- Generators
----------------------------------------------------------------------------

-- The data we need to mint an Edition Run and distribute editions.
data EditionRunData = EditionRunData
  { dataEditionId :: EditionId
  , dataEditionInfo :: EditionInfo
  , dataNumberOfEditions :: Natural -- ^ How many editions exist in this edition run
  , dataNumberToDistribute :: Natural -- ^ How many editions we will be giving out
  }
  deriving stock (Show)

genEditionRunData :: EditionId -> Gen EditionRunData
genEditionRunData editionId = do
  editionInfo <- genEditionInfo
  numberOfEditions <- Gen.integral (Range.linear 0 30)
  toDistribute <- Gen.integral (Range.linear 0 numberOfEditions)
  pure $ EditionRunData editionId editionInfo numberOfEditions toDistribute

genEditionRunDataList :: Gen [EditionRunData]
genEditionRunDataList = do
  editionRunCount <- Gen.int (Range.linear 0 10)

  -- Edition IDs are sequential, starting at 0
  forM [0.. editionRunCount - 1] \editionId ->
    genEditionRunData (fromIntegral editionId)

genEditionInfo :: Gen EditionInfo
genEditionInfo =
  Gen.map (Range.linear 0 10) (liftA2 (,) genKeys genValues)
  where
    genKeys = Gen.genMText
    genValues = Gen.bytes (Range.linear 0 10)
