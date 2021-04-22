-- | Lorentz bindings for the fixed price sale contract
-- with allowlist.
module Lorentz.Contracts.EnglishAuction.Allowlisted where

import Lorentz

assetNotAllowed :: MText
assetNotAllowed = [mt|ASSET_NOT_ALLOWED|]
