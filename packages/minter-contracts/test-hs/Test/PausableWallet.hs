module Test.PausableWallet where 

import Lorentz.Contracts.PausableWallet
import Lorentz.Value
import qualified Michelson.Typed as T
import Morley.Nettest
import Test.SimpleAdmin
import Test.Tasty (TestTree)

test_AdminChecks :: TestTree
test_AdminChecks =
  adminOwnershipTransferChecks originatePausableWallet

originatePausableWallet
  :: MonadNettest caps base m
  => Address
  -> m (TAddress PausableWalletEntrypoints)
originatePausableWallet admin = do
  TAddress <$> originateUntypedSimple "pausable-wallet"
    (T.untypeValue $ T.toVal $
      initPausableWalletStorage admin)
    (T.convertContract pausableWalletContract)
