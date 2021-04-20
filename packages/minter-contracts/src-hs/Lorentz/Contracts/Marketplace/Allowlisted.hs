-- | Lorentz bindings for the fixed price sale contract
-- with allowlist.
module Lorentz.Contracts.Marketplace.Allowlisted where

import Lorentz

-- Errors
----------------------------------------------------------------------------

saleAddressNotAllowed :: MText
saleAddressNotAllowed = [mt|SALE_ADDRESS_NOT_ALLOWED|]

moneyAddressNotAllowed :: MText
moneyAddressNotAllowed = [mt|MONEY_ADDRESS_NOT_ALLOWED|]
