{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Protocol/versioning related communication helpers.

module Pos.Communication.Protocol
       ( module Pos.Communication.Types.Protocol
       , listenerOneMsg
       , listenerConv
       , hoistSendActions
       , mapListener
       , mapListener'
       , mapActionSpec
       , Message (..)
       , MessageName (..)
       , messageName'
       , worker
       , worker'
       , localWorker
       , toAction
       , unpackLSpecs
       , hoistListenerSpec
       , onNewSlotWorker
       , localOnNewSlotWorker
       , onNewSlotWithLoggingWorker
       , convertSendActions
       , protocolListeners
       ) where

import           Control.Arrow                    ((&&&))
import qualified Data.HashMap.Strict              as HM
import           Data.Proxy                       (Proxy)
import           Formatting                       (build, sformat, shown, stext, (%))
import           Mockable                         (Delay, Fork, Mockable, SharedAtomic,
                                                   Throw, modifySharedAtomic,
                                                   readSharedAtomic, throw)
import qualified Node                             as N
import           Node.Message                     (Message (..), MessageName (..),
                                                   messageName')
import           System.Wlog                      (WithLogger, logDebug, logWarning)
import           Universum

import           Pos.Binary.Class                 (Bi)
import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.PeerState      (WithPeerState (..), peerVerInfo)
import           Pos.Communication.Types.Protocol
import           Pos.Core.Types                   (SlotId)
import           Pos.DHT.Model.Class              (MonadDHT)
import           Pos.Reporting.Class              (MonadReportingMem)
import           Pos.Slotting.Class               (MonadSlots)
import           Pos.Slotting.Util                (onNewSlot, onNewSlotImpl)

protocolListeners :: (Bi NOP, Message NOP, WithLogger m) => [Listener m]
protocolListeners =
    [N.ListenerActionConversation $
        \(peerId, _) nodeId (conv :: N.ConversationActions NOP NOP m) -> do
            void $ N.recv conv
            logDebug $ sformat ("Received NOP from "%build) (NodeId (peerId, nodeId))
    ]

mapListener
    :: (forall t. m t -> m t) -> Listener m -> Listener m
mapListener = mapListener' identity $ const identity

mapListener'
    :: (N.SendActions BiP PeerData m -> N.SendActions BiP PeerData m)
    -> (forall snd rcv. Message rcv => N.NodeId
          -> N.ConversationActions snd rcv m
          -> N.ConversationActions snd rcv m)
    -> (forall t. m t -> m t) -> Listener m -> Listener m
mapListener' saMapper _ mapper (N.ListenerActionOneMsg f) =
    N.ListenerActionOneMsg $ \d nId sA -> mapper . f d nId (saMapper sA)
mapListener' _ caMapper mapper (N.ListenerActionConversation f) =
    N.ListenerActionConversation $ \d nId -> mapper . f d nId . caMapper nId

mapActionSpec
    :: (N.SendActions BiP PeerData m -> N.SendActions BiP PeerData m)
    -> (forall t. m t -> m t) -> ActionSpec m a -> ActionSpec m a
mapActionSpec saMapper aMapper (ActionSpec f) =
    ActionSpec $ \vI sA -> aMapper $ f vI (saMapper sA)

hoistConversationActions
    :: (forall a. n a -> m a)
    -> ConversationActions body rcv n
    -> ConversationActions body rcv m
hoistConversationActions nat ConversationActions {..} =
    ConversationActions (nat . send) (nat recv)

hoistSendActions
    :: (forall a. n a -> m a)
    -> (forall a. m a -> n a)
    -> SendActions n
    -> SendActions m
hoistSendActions nat rnat SendActions {..} = SendActions sendTo' withConnectionTo'
  where
    sendTo' nodeId msg = nat $ sendTo nodeId msg
    withConnectionTo' nodeId convActionsH =
        nat $ withConnectionTo nodeId $ \peerData convActions ->
        rnat $ convActionsH (nat peerData) $ hoistConversationActions nat convActions

hoistListenerSpec
    :: (forall a. m a -> n a)
    -> (forall a. n a -> m a)
    -> ListenerSpec m
    -> ListenerSpec n
hoistListenerSpec nat rnat (ListenerSpec h s) =
    ListenerSpec (\vI -> N.hoistListenerAction nat rnat $ h vI) s

convertCA :: N.ConversationActions snd rcv m -> ConversationActions snd rcv m
convertCA cA = ConversationActions
    { send = N.send cA
    , recv = N.recv cA
    }

convertSendActions
    :: ( WithLogger m
       , Mockable Throw m
       , WithPeerState m
       , Mockable SharedAtomic m
       , Bi NOP
       , Message NOP
       )
    => VerInfo -> N.SendActions BiP PeerData m -> SendActions m
convertSendActions ourVerInfo sA = modifySend (vIOutHandlers ourVerInfo) $
  SendActions
      { sendTo = \(NodeId (_peerId, nNodeId)) -> N.sendTo sA nNodeId
      , withConnectionTo = \(NodeId (_peerId, nNodeId)) h ->
          let h' = \peerData conversationActions -> h peerData (convertCA conversationActions)
          in N.withConnectionTo sA nNodeId $ h'
      }

listenerOneMsg
    :: ( Bi msg
       , Message msg
       , WithLogger m
       , Mockable Throw m
       , WithPeerState m
       , Mockable SharedAtomic m
       , Bi NOP
       , Message NOP
       )
    => OutSpecs
    -> (VerInfo -> NodeId -> SendActions m -> msg -> m ())
    -> (ListenerSpec m, OutSpecs)
listenerOneMsg outSpecs h = (lspec, outSpecs)
  where
    lspec = flip ListenerSpec spec $
              \ourVerInfo -> N.ListenerActionOneMsg $
                  \(peerId, peerVerInfo') nNodeId sA msg -> do
                      setPeerVerInfo peerId peerVerInfo'
                      checkingInSpecs ourVerInfo peerVerInfo' spec peerId $
                          h ourVerInfo
                            (NodeId (peerId, nNodeId))
                            (convertSendActions ourVerInfo sA)
                            msg
    msgProxy :: (a -> b -> d -> msg -> c) -> Proxy msg
    msgProxy _ = Proxy
    spec = (rcvMsgName, OneMsgHandler)
    rcvMsgName = messageName $ msgProxy h

listenerConv
    :: ( Bi snd
       , Bi rcv
       , Message snd
       , Message rcv
       , WithLogger m
       , WithPeerState m
       , Mockable SharedAtomic m
       )
    => (VerInfo -> NodeId -> ConversationActions snd rcv m -> m ())
    -> (ListenerSpec m, OutSpecs)
listenerConv h = (lspec, mempty)
  where
    spec = (rcvMsgName, ConvHandler sndMsgName)
    lspec = flip ListenerSpec spec $
              \ourVerInfo -> N.ListenerActionConversation $
                \(peerId, peerVerInfo') nNodeId conv -> do
                    setPeerVerInfo peerId peerVerInfo'
                    checkingInSpecs ourVerInfo peerVerInfo' spec peerId $
                        h ourVerInfo
                          (NodeId (peerId, nNodeId))
                          (convertCA conv)
    convProxy = convProxy' h
    convProxy' :: (a -> b -> c -> d) -> Proxy c
    convProxy' _ = Proxy
    sndMsgName = messageName $ sndProxy convProxy
    rcvMsgName = messageName $ rcvProxy convProxy

setPeerVerInfo :: (WithPeerState m, Mockable SharedAtomic m) => PeerId -> VerInfo -> m ()
setPeerVerInfo peerId verInfo = do
    stV <- getPeerState peerId
    modifySharedAtomic stV $ \st -> return (set peerVerInfo (Just verInfo) st, ())

unpackLSpecs :: ([ListenerSpec m], OutSpecs) -> (VerInfo -> [Listener m], InSpecs, OutSpecs)
unpackLSpecs =
    over _1 (\ls verInfo -> map ($ verInfo) ls) .
    over _2 (InSpecs . HM.fromList) .
    convert . first (map lsToPair)
  where
    lsToPair (ListenerSpec h spec) = (h, spec)
    convert :: Monoid out => ([(l, i)], out) -> ([l], [i], out)
    convert = uncurry (uncurry (,,))
                . first squashPL
    squashPL :: [(a, b)] -> ([a], [b])
    squashPL = map fst &&& map snd


type WorkerConstr m =
    ( WithLogger m
    , Mockable Throw m
    , WithPeerState m
    , Mockable SharedAtomic m
    , Bi NOP
    , Message NOP
    )

toAction
    :: WorkerConstr m
    => (SendActions m -> m a) -> ActionSpec m a
toAction h = ActionSpec $ \vI -> h . convertSendActions vI

worker
    :: WorkerConstr m
    => OutSpecs -> Worker' m -> (WorkerSpec m, OutSpecs)
worker outSpecs = (,outSpecs) . toAction

workerHelper
    :: WorkerConstr m
    => OutSpecs -> (arg -> Worker' m) -> (arg -> WorkerSpec m, OutSpecs)
workerHelper outSpecs h = (,outSpecs) $ toAction . h

worker'
    :: WorkerConstr m
    => OutSpecs -> (VerInfo -> Worker' m) -> (WorkerSpec m, OutSpecs)
worker' outSpecs h =
    (,outSpecs) $ ActionSpec $ \vI -> h vI . convertSendActions vI


type OnNewSlotComm m =
    ( MonadIO m
    , MonadSlots m
    , MonadMask m
    , WithLogger m
    , Mockable Fork m
    , Mockable Delay m
    , Mockable Throw m
    , WithPeerState m
    , Mockable SharedAtomic m
    , Bi NOP
    , Message NOP
    , MonadDHT m
    , MonadReportingMem m
    )

onNewSlot'
    :: OnNewSlotComm m
    => Bool -> Bool -> (SlotId -> WorkerSpec m, outSpecs) -> (WorkerSpec m, outSpecs)
onNewSlot' withLog startImmediately (h, outs) =
    (,outs) . ActionSpec $ \vI sA ->
        onNewSlotImpl withLog startImmediately $
            \slotId -> let ActionSpec h' = h slotId
                        in h' vI sA
onNewSlotWorker
    :: OnNewSlotComm m
    => Bool -> OutSpecs -> (SlotId -> Worker' m) -> (WorkerSpec m, OutSpecs)
onNewSlotWorker b outs = onNewSlot' False b . workerHelper outs

onNewSlotWithLoggingWorker
    :: OnNewSlotComm m
    => Bool -> OutSpecs -> (SlotId -> Worker' m) -> (WorkerSpec m, OutSpecs)
onNewSlotWithLoggingWorker b outs = onNewSlot' True b . workerHelper outs

localOnNewSlotWorker
    :: ( MonadIO m
       , MonadSlots m
       , MonadMask m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       , MonadDHT m
       , MonadReportingMem m
       ) => Bool -> (SlotId -> m ()) -> (WorkerSpec m, OutSpecs)
localOnNewSlotWorker b h = (ActionSpec $ \__vI __sA -> onNewSlot b h, mempty)

localWorker :: m () -> (WorkerSpec m, OutSpecs)
localWorker h = (ActionSpec $ \__vI __sA -> h, mempty)

checkingInSpecs
    :: WithLogger m
    => VerInfo
    -> VerInfo
    -> (MessageName, HandlerSpec)
    -> PeerId
    -> m ()
    -> m ()
checkingInSpecs ourVerInfo peerVerInfo' spec peerId action =
    if | spec `notInSpecs` vIInHandlers ourVerInfo ->
              logWarning $ sformat
                ("Endpoint is served, but not reported " % build) spec
       | spec `notInSpecs` vIOutHandlers peerVerInfo' ->
              logDebug $ sformat
                ("Peer " % build % " attempting to use endpoint he didn't report to use " % build)
                peerId spec
       | otherwise -> action

rcvProxy :: Proxy (ConversationActions snd rcv m) -> Proxy rcv
rcvProxy _ = Proxy

sndProxy :: Proxy (ConversationActions snd rcv m) -> Proxy snd
sndProxy _ = Proxy

data SpecError = OutSpecNotReported (MessageName, HandlerSpec)
               | PeerInSpecNotReported (MessageName, HandlerSpec)
  deriving (Generic, Show)

instance Exception SpecError

modifySend :: ( WithLogger m, Mockable Throw m
              , WithPeerState m
              , Mockable SharedAtomic m
              , Bi NOP
              , Message NOP
              )
           => HandlerSpecs -> SendActions m -> SendActions m
modifySend ourOutSpecs sA = sA
    { sendTo = \nodeId msg -> do
          logDebug $ sformat ("Sending message "%stext%" to "%build%" ...")
                          (formatMessage msg) nodeId
          pVI <- acquirePVI nodeId
          let sndMsgName = messageName' msg
          checkingOutSpecs (sndMsgName, OneMsgHandler)
              nodeId (vIInHandlers pVI) $ do
                sendTo sA nodeId msg
                logDebug $ sformat ("Message "%stext%" to "%build%" sent")
                                (formatMessage msg) nodeId
    , withConnectionTo = \nodeId conv -> do
          pVI <- acquirePVI nodeId
          let sndMsgName = messageName . sndProxy $ sndArgProxy conv
              rcvMsgName = messageName . rcvProxy $ sndArgProxy conv
          checkingOutSpecs (sndMsgName, ConvHandler rcvMsgName)
              nodeId (vIInHandlers pVI) $ withConnectionTo sA nodeId conv
    }
  where
    requestPVI nodeId = withConnectionTo sA nodeId $
      \peerData (conv :: ConversationActions NOP NOP m) -> do
          send conv NOP
          logDebug $ sformat ("Sent NOP to "%build) nodeId
          snd <$> peerData
    acquirePVI nodeId@(NodeId (peerId, _)) = do
        st <- getPeerState peerId
        mVI <- view peerVerInfo <$> readSharedAtomic st
        case mVI of
            Just vi -> pure vi
            _ -> do
                logDebug $ sformat ("No verInfo in state for "%build%", requesting...") nodeId
                vi <- requestPVI nodeId
                logDebug $ sformat ("VerInfo: "%build%" (peer "%build%")")
                                vi nodeId
                modifySharedAtomic st $ \peerState ->
                  return (set peerVerInfo (Just vi) peerState, vi)
    sndArgProxy :: (a -> b -> c) -> Proxy b
    sndArgProxy _ = Proxy

    checkingOutSpecs spec (NodeId (peerId, _)) peerInSpecs action = do
        if | spec `notInSpecs` ourOutSpecs -> do
                  logWarning $ sformat
                     ("Sending "%build%": endpoint not reported")
                     spec
                  throw $ OutSpecNotReported spec
           | spec `notInSpecs` peerInSpecs -> do
                  logDebug $ sformat
                     ("Attempting to send to "%build%": endpoint unsupported by peer "%shown)
                     spec peerId
                  throw $ PeerInSpecNotReported spec
           | otherwise -> action
