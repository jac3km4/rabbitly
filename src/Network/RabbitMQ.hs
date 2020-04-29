{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Network.RabbitMQ
  ( Subscription (..),
    Publisher,
    Message (..),
    Delivery (..),
    RoutingKey (..),
    Credentials (..),
    Address (..),
    BasicProperties (..),
    ConsumeFlags (..),
    ExchangeName (..),
    QueueName (..),
    defaultProperties,
    chanPublisher,
    publish,
    runPubSub,
    runPubSubS,
  )
where

import Control.Arrow ((&&&))
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Monad (forM)
import Control.Monad.Catch (throwM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Function ((&))
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Serialize as Serialize
import Network.RabbitMQ.Frame (ChannelId (..), ContentHeader (..), Frame (..), framing, unframing)
import Network.RabbitMQ.Property (Property (..))
import Network.RabbitMQ.Protocol
import Network.RabbitMQ.Types
import Network.Socket (Socket)
import Streamly (IsStream, (|&))
import Streamly.Data.Fold (Fold)
import qualified Streamly.External.ByteString as Strict
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Internal.Network.Inet.TCP as TCP
import qualified Streamly.Internal.Network.Socket as SK
import qualified Streamly.Internal.Prelude as S

data Subscription m
  = Subscription
      { subscriptionQueue :: QueueName,
        subscriptionFlags :: ConsumeFlags,
        subscription :: Fold m Delivery ()
      }

newtype Publisher
  = Publisher
      { channel :: Chan Message
      }

chanPublisher :: IO Publisher
chanPublisher = Publisher <$> Chan.newChan

publish :: Publisher -> Message -> IO ()
publish (Publisher chan) = Chan.writeChan chan

open :: Socket -> Credentials -> IO TuneOk
open sock credentials = do
  SK.writeChunk sock $ Strict.toArray header
  readFrame sock >>= \case
    MethodFrame _ (StartMethod payload) -> do
      let encoded = "\0" <> user credentials <> "\0" <> password credentials
      call sock defaultChannel (StartOkMethod (StartOk defaultPeerProperties "PLAIN" encoded (startLocales payload)))
    other -> throwM $ ProtocolException $ "Unexpected start message response: " <> show other
  tuneResult <- readFrame sock >>= \case
    MethodFrame _ (TuneMethod payload) -> do
      let response = TuneOk maxChannels (tuneFrameMax payload) heartbeatRate
      call sock defaultChannel (TuneOkMethod response)
      call sock defaultChannel (OpenMethod (Open (Path "/") "" False))
      pure response
    other -> throwM $ ProtocolException $ "Unexpected tune message response: " <> show other
  readFrame sock >>= \case
    MethodFrame _ (OpenOkMethod _) -> pure ()
    other -> throwM $ ProtocolException $ "Unexpected open connection response: " <> show other
  pure tuneResult
  where
    defaultChannel = ChannelId 0
    header = "AMQP\0\0\9\1"
    maxChannels = 1
    heartbeatRate = 0

runPubSub :: Address -> Credentials -> Publisher -> [Subscription IO] -> IO ThreadId
runPubSub addr credentials publisher subscriptions =
  forkIO $ S.drain $ runPubSubS addr credentials publisher subscriptions

runPubSubS :: (IsStream t) => Address -> Credentials -> Publisher -> [Subscription IO] -> t IO ()
runPubSubS (Address ip port) credentials publisher subscriptions =
  TCP.withConnection ip port $ \sock -> S.concatM $ do
    options <- open sock credentials
    call sock subChannel (OpenChannelMethod (OpenChannel ""))
    readFrame sock >>= \case
      MethodFrame _ (OpenChannelOkMethod _) -> pure ()
      other -> throwM $ ProtocolException $ "Unexpected open channel response: " <> show other
    consumers <- forM subscriptions $ \sub -> do
      call sock subChannel (ConsumeMethod (Consume 0 (subscriptionQueue sub) (ConsumerTag "") (subscriptionFlags sub) mempty))
      readFrame sock >>= \case
        MethodFrame _ (ConsumeOkMethod (ConsumeOk tag)) ->
          pure (tag, subscription sub)
        other -> throwM $ ProtocolException $ "Unexpected consume response: " <> show other
    let maxFrameSize = fromIntegral (tuneOkFrameMax options)
    pure $ S.parallelFst (consumption sock consumers) (publishing sock subChannel maxFrameSize)
  where
    subChannel = ChannelId 1 -- currently everything happens on channel 1
    consumption sock consumers =
      S.unfold SK.read sock
        & S.scan unframing
        & S.mapMaybe id
        & S.scan deliveries
        & S.mapMaybe id
        & S.map (consumerTag &&& id)
        |& S.scan (FL.demux_ (Map.fromList consumers))
    publishing sock channelId maxFrameSize =
      S.repeatM (Chan.readChan (channel publisher))
        & S.concatMap (framing maxFrameSize channelId)
        & S.map (Strict.toArray . Serialize.encode)
        & S.scan (SK.writeChunks sock)

call :: Socket -> ChannelId -> Method -> IO ()
call socket channelId =
  SK.writeChunk socket . Strict.toArray . Serialize.encode . MethodFrame channelId

readFrame :: Socket -> IO Frame
readFrame sock = do
  res <- S.unfold SK.read sock & S.scan unframing & S.mapMaybe id & S.head
  maybe (throwM (ProtocolException "Unexpected EOF")) pure res

data DeliveryState
  = AwaitingDelivery
  | ReceivedDelivery Deliver
  | ReceivedHeader Deliver ContentHeader
  | ReceivedBody Deliver ContentHeader ByteString

deliveries :: Monad m => Fold m Frame (Maybe Delivery)
deliveries = FL.mkPure match AwaitingDelivery extract
  where
    match _ (MethodFrame _ (DeliverMethod delivery)) =
      ReceivedDelivery delivery
    match (ReceivedDelivery delivery) (HeaderFrame _ header) =
      ReceivedHeader delivery header
    match (ReceivedHeader delivery header) (BodyFrame _ body) =
      ReceivedBody delivery header body
    match (ReceivedBody delivery header body) (BodyFrame _ body') =
      ReceivedBody delivery header (body <> body')
    match state _ = state
    extract (ReceivedBody delivery header body)
      | BS.length body == fromIntegral (bodySize header) =
        Just $ Delivery (deliverConsumerTag delivery) (deliverDeliveryTag delivery) body
      | otherwise = Nothing
    extract _ = Nothing

defaultPeerProperties :: PeerProperties
defaultPeerProperties =
  PeerProperties $
    HashMap.fromList
      [ ("product", PropLongStr "RabbitMQ"),
        ("version", PropLongStr "3.8.3"),
        ("platform", PropLongStr "rabbitly Haskell"),
        ("copyright", PropLongStr "jac3km4"),
        ("information", PropLongStr "BSD3 Licensed"),
        ("capabilities", PropTable mempty)
      ]
