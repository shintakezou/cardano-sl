{-# LANGUAGE MultiParamTypeClasses #-}
module Pos.DHT.Workers
       ( dhtWorkers
       ) where

import           Data.Binary                (encode)
import qualified Data.ByteString.Lazy       as BS
import           Formatting                 (sformat, (%))
import           Network.Kademlia           (takeSnapshot)
import           System.Wlog                (logNotice)
import           Universum

import           Pos.Binary.DHTModel        ()
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, localOnNewSlotWorker)
import           Pos.Constants              (kademliaDumpInterval)
import           Pos.Context                (bpKademliaDump, getNodeContext, ncNodeParams,
                                             npBaseParams)
import           Pos.DHT.Real.Types         (KademliaDHTInstance (..),
                                             WithKademliaDHTInstance (..))
import           Pos.Types                  (flattenSlotId, slotIdF)
import           Pos.WorkMode               (WorkMode)

dhtWorkers :: (WorkMode ssc m) => ([WorkerSpec m], OutSpecs)
dhtWorkers = first pure dumpKademliaStateWorker

dumpKademliaStateWorker :: WorkMode ssc m => (WorkerSpec m, OutSpecs)
dumpKademliaStateWorker = localOnNewSlotWorker True $ \slotId ->
    when (flattenSlotId slotId `mod` kademliaDumpInterval == 0) $ do
        dumpFile <- bpKademliaDump . npBaseParams . ncNodeParams <$> getNodeContext
        logNotice $ sformat ("Dumping kademlia snapshot on slot: "%slotIdF) slotId
        inst <- kdiHandle <$> getKademliaDHTInstance
        snapshot <- liftIO $ takeSnapshot inst
        liftIO . BS.writeFile dumpFile $ encode snapshot
