-- | <https://tools.ietf.org/html/rfc4511#section-4.11 Abandon> operation.
--
-- This operation comes in two flavours:
--
--   * asynchronous, 'IO' based ('abandonAsync')
--
--   * asynchronous, 'STM' based ('abandonAsyncSTM')
--
-- Of those, the first one ('abandonAsync') is probably the most useful for the typical usecase.
--
-- Synchronous variants are unavailable because the Directory does not
-- respond to @AbandonRequest@s.
module Ldap.Client.Abandon
  ( abandonAsync
  , abandonAsyncSTM
  ) where

import           Control.Monad (void)
import           Control.Monad.STM (STM, atomically)

import qualified Ldap.Asn1.Type as Type
import           Ldap.Client.Internal


-- | Perform the Abandon operation asynchronously.
abandonAsync :: Ldap -> Async a -> IO ()
abandonAsync l =
  atomically . abandonAsyncSTM l

-- | Perform the Abandon operation asynchronously.
abandonAsyncSTM :: Ldap -> Async a -> STM ()
abandonAsyncSTM l =
  void . sendRequest l die . abandonRequest
 where
  die = error "Ldap.Client.Abandon: do not wait for the response to UnbindRequest"

abandonRequest :: Async a -> Request
abandonRequest (Async i _)  =
  Type.AbandonRequest i
