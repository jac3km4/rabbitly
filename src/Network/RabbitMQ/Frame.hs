{-# LANGUAGE StrictData #-}

module Network.RabbitMQ.Frame where

import qualified ByteString.StrictBuilder as Builder
import ByteString.StrictBuilder (Builder)
import Control.Monad.Catch (MonadThrow (..))
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.Serialize as Serialize
import Data.Serialize (Serialize (..))
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.Put as Put
import Data.Word (Word16, Word32, Word64, Word8)
import qualified Network.RabbitMQ.Protocol as Proto
import Network.RabbitMQ.Types (Message (..), RabbitException (..), RoutingKey (..))
import Streamly (IsStream)
import Streamly.Data.Fold (Fold)
import qualified Streamly.Internal.Data.Fold as FL
import qualified Streamly.Prelude as S

data Frame
  = MethodFrame ChannelId Proto.Method
  | HeaderFrame ChannelId ContentHeader
  | BodyFrame ChannelId ByteString
  | Heartbeat
  deriving (Show)

instance Serialize Frame where
  put frame =
    case frame of
      MethodFrame channel method ->
        wrapped 1 channel (Serialize.encode method)
      HeaderFrame channel header ->
        wrapped 2 channel (Serialize.encode header)
      BodyFrame channel bs ->
        wrapped 3 channel bs
      Heartbeat ->
        wrapped 4 (ChannelId 0) mempty
    where
      wrapped type' channel bs = do
        put $ FrameHeader type' channel $ fromIntegral $ BS.length bs
        Put.putByteString bs
        put (0xCE :: Word8)
  get = undefined

data ContentHeader
  = ContentHeader
      { classId :: Word16,
        weight :: Word16,
        bodySize :: Word64,
        properties :: Proto.BasicProperties
      }
  deriving (Show)

instance Serialize ContentHeader where
  put ContentHeader {classId, weight, bodySize, properties} = do
    Put.putWord16be classId
    Put.putWord16be weight
    Put.putWord64be bodySize
    put properties
  get = do
    classId <- Get.getWord16be
    weight <- Get.getWord16be
    bodySize <- Get.getWord64be
    properties <- get
    pure $ ContentHeader {classId, weight, bodySize, properties}

data FrameHeader
  = FrameHeader
      { frameType :: Word8,
        channel :: ChannelId,
        frameSize :: Word32
      }
  deriving (Show)

instance Serialize FrameHeader where
  put FrameHeader {frameType, channel, frameSize} =
    put frameType *> put channel *> Put.putWord32be frameSize
  get = FrameHeader <$> get <*> get <*> Get.getWord32be

newtype ChannelId = ChannelId Word16
  deriving (Show)

instance Serialize ChannelId where
  put (ChannelId val) = Put.putWord16be val
  get = ChannelId <$> Get.getWord16be

data FrameState
  = CollectingHeader Builder Word32
  | CollectingBody FrameHeader Builder Word32

unframing :: MonadThrow m => Fold m Word8 (Maybe Frame)
unframing = FL.mkFold go initial extract
  where
    go (CollectingHeader acc n) el
      | n == 0 = do
        header <- decode $ Builder.builderBytes acc
        pure $ CollectingBody header (Builder.word8 el) (frameSize header)
      | otherwise = pure $ CollectingHeader (acc <> Builder.word8 el) (n - 1)
    go (CollectingBody header acc n) el
      | n == 0 = pure $ CollectingHeader (Builder.word8 el) 6
      | n == 1 = pure $ CollectingBody header acc (n - 1) -- skip the last byte
      | otherwise = pure $ CollectingBody header (acc <> Builder.word8 el) (n - 1)
    extract (CollectingBody FrameHeader {channel, frameType} acc 0) =
      let bytes = Builder.builderBytes acc
       in case frameType of
            1 -> Just . MethodFrame channel <$> decode bytes
            2 -> Just . HeaderFrame channel <$> decode bytes
            3 -> pure . Just $ BodyFrame channel bytes
            4 -> pure $ Just Heartbeat
            other -> throwM $ ProtocolException $ "Unexpected frame type: " <> show other
    extract _ = pure Nothing
    decode :: MonadThrow m => Serialize a => ByteString -> m a
    decode = either (throwM . FormatException) pure . Serialize.decode
    initial = pure $ CollectingHeader mempty 7

framing :: (IsStream t, Monad m) => Int -> ChannelId -> Message -> t m Frame
framing _ channel (Ack tag) =
  S.yield $ MethodFrame channel $ Proto.AckMethod $ Proto.Ack tag False
framing _ channel (Reject tag requeue) =
  S.yield $ MethodFrame channel $ Proto.RejectMethod $ Proto.Reject tag requeue
framing maxFrameSize channel (Envelope exchange (RoutingKey routingKey) payload properties) =
  let bodyFrames = BodyFrame channel <$> splitFrames payload
   in S.fromList $
        MethodFrame channel (Proto.PublishMethod (Proto.Publish 0 exchange routingKey flags))
          : HeaderFrame channel (ContentHeader 60 0 size properties)
          : bodyFrames
  where
    flags = Proto.PublishFlags False False
    size = fromIntegral $ BS.length payload
    splitFrames bs
      | BS.length bs > maxFrameSize =
        let (x, xs) = BS.splitAt maxFrameSize bs
         in x : splitFrames xs
      | BS.length bs > 0 = [bs]
      | otherwise = []

-- unframing :: MonadThrow m => Parser m Word8 Frame
-- unframing = do
--   headerBytes <- PR.take headerSize (FL.foldMap BS.singleton)
--   header <- either PR.die (pure . traceShowId) $ P.decode headerBytes
--   frame <- PR.take (fromIntegral (frameSize header)) (FL.foldMap BS.singleton)
--   _ <- PR.satisfy (== 0xCE)
--   case frameType header of
--     1 -> MethodFrame (channel header) <$> either PR.die pure (P.decode frame)
--     2 -> HeaderFrame (channel header) <$> either PR.die pure (P.decode frame)
--     3 -> pure $ BodyFrame (channel header) frame
--     4 -> pure Heartbeat
--     other -> PR.die $ "Unexpected frame type: " <> show other
--   where
--     headerSize = 7
