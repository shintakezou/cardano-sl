{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods (for Pos.Util.Relay based methods).

module Pos.Communication.Methods
       ( sendTx
       , sendTx'
       ) where

import           Formatting                  (build, sformat, shown, (%))
import           Mockable                    (handleAll)
import           Node                        (SendActions)
import           System.Wlog                 (logWarning)
import           Universum

import           Pos.Binary.Communication    ()
import           Pos.Binary.Relay            ()
import           Pos.Binary.Types            ()
import           Pos.Communication.BiP       (BiP)
import           Pos.Context                 (WithNodeContext)
import           Pos.Crypto                  (hash)
import           Pos.DHT.Model.Neighbors     (getPreferredInterface, sendToNode,
                                              sendToNode', setPreferredInterface)
import           Pos.Txp.Types.Communication (TxMsgContents (..))
import           Pos.Types                   (TxAux)
import           Pos.Util.Relay              (DataMsg (..))
import           Pos.Util.TimeWarp           (NetworkAddress)
import           Pos.WorkMode                (MinWorkMode)

-- | Send Tx to given address.
sendTx
    :: (WithNodeContext ssc m, MinWorkMode m)
    => SendActions BiP m -> NetworkAddress -> TxAux -> m ()
sendTx = sendTx' $ Just (setPreferredInterface, getPreferredInterface)

-- | Like 'sendTx', but can be used outside of node context.
sendTx'
    :: MinWorkMode m
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions BiP m -> NetworkAddress -> TxAux -> m ()
sendTx' mbPrefs sendActions addr (tx,w,d) = handleAll handleE $ do
    sendToNode' mbPrefs sendActions addr $ DataMsg (TxMsgContents tx w d) (hash tx)
  where
    handleE e = logWarning $ sformat ("Error sending tx "%build%" to "%shown%": "%shown) tx addr e
