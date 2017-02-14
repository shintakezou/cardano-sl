-- | Sending PSK-related datatypes to network.

module Pos.Delegation.Methods
       ( sendProxySKEpoch
       , sendProxySKEpoch'
       , sendProxySKSimple
       , sendProxySKSimple'
       , sendProxyConfirmSK
       ) where

import           Formatting               (build, sformat, (%))
import           Node                     (SendActions)
import           System.Wlog              (logDebug)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Communication.BiP    (BiP)
import           Pos.Context              (WithNodeContext (..), ncSecretKey)
import           Pos.Crypto               (proxySign)
import           Pos.Delegation.Types     (ConfirmProxySK (..), SendProxySK (..))
import           Pos.DHT.Model            (getPreferredInterface, sendToNeighbors,
                                           sendToNeighbors', setPreferredInterface)
import           Pos.Types                (ProxySKEpoch, ProxySKSimple)
import           Pos.Util.TimeWarp        (NetworkAddress)
import           Pos.WorkMode             (MinWorkMode, WorkMode)

-- | Sends epoch psk to neighbours
sendProxySKEpoch
    :: (WithNodeContext ssc m, MinWorkMode m)
    => SendActions BiP m -> ProxySKEpoch -> m ()
sendProxySKEpoch = sendProxySKEpoch' $
    Just (setPreferredInterface, getPreferredInterface)

sendProxySKEpoch'
    :: MinWorkMode m
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions BiP m -> ProxySKEpoch -> m ()
sendProxySKEpoch' mbPrefs sendActions psk = do
    logDebug $ sformat ("Sending lightweight psk to neigbours:\n"%build) psk
    -- [CSL-514] TODO Log long acting sends
    sendToNeighbors' mbPrefs sendActions $ SendProxySKEpoch psk

-- | Sends simple psk to neighbours
sendProxySKSimple
    :: (WithNodeContext ssc m, MinWorkMode m)
    => SendActions BiP m -> ProxySKSimple -> m ()
sendProxySKSimple = sendProxySKSimple' $
    Just (setPreferredInterface, getPreferredInterface)

sendProxySKSimple'
    :: MinWorkMode m
    => Maybe (NetworkAddress -> Word32 -> m (),
              NetworkAddress -> m Word32)
    -> SendActions BiP m -> ProxySKSimple -> m ()
sendProxySKSimple' mbPrefs sendActions psk = do
    logDebug $ sformat ("Sending heavyweight psk to neigbours:\n"%build) psk
    -- [CSL-514] TODO Log long acting sends
    sendToNeighbors' mbPrefs sendActions $ SendProxySKSimple psk

-- | Generates a proof of being a delegate for psk and sends it to
-- neighbors.
sendProxyConfirmSK
    :: (WorkMode ss m)
    => SendActions BiP m -> ProxySKEpoch -> m ()
sendProxyConfirmSK sendActions pSk = do
    logDebug $
        sformat ("Generating delivery proof and propagating it to neighbors: "%build) pSk
    sk <- ncSecretKey <$> getNodeContext
    let proof = proxySign sk pSk pSk -- but still proving is nothing but fear
    -- [CSL-514] TODO Log long acting sends
    sendToNeighbors sendActions $ ConfirmProxySK pSk proof
