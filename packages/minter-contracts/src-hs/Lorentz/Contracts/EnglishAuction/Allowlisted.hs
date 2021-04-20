-- | Lorentz bindings for the fixed price sale contract
-- with allowlist.
module Lorentz.Contracts.EnglishAuction.Allowlisted where

import Lorentz

assetAddressNotAllowed :: MText
assetAddressNotAllowed = [mt|ASSET_ADDRESS_NOT_ALLOWED|]
