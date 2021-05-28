-- | Simple allowlisting implementation.
--
-- This module is to be imported qualified.
module Lorentz.Contracts.AllowlistSimple where

import Lorentz

type Allowlist = BigMap Address ()

type Entrypoints = Allowlist
