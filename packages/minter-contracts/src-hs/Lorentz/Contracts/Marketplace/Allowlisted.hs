-- | Lorentz bindings for the fixed price sale contract
-- with allowlist.
module Lorentz.Contracts.Marketplace.Allowlisted where

import Lorentz

-- Errors
----------------------------------------------------------------------------

saleTokenNotAllowed :: MText
saleTokenNotAllowed = [mt|SALE_TOKEN_NOT_ALLOWED|]
