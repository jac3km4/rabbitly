{-# LANGUAGE StrictData #-}

module Network.RabbitMQ.Protocol where

import Data.Bits ((.|.), shiftL, testBit)
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Maybe (isJust)
import Data.Serialize (Serialize (..))
import qualified Data.Serialize.Get as Get
import qualified Data.Serialize.Put as Put
import Data.Text (Text)
import Network.RabbitMQ.Property

-- the contents of this file were generated

data Method
  = StartMethod Start
  | StartOkMethod StartOk
  | SecureMethod Secure
  | SecureOkMethod SecureOk
  | TuneMethod Tune
  | TuneOkMethod TuneOk
  | OpenMethod Open
  | OpenOkMethod OpenOk
  | CloseMethod Close
  | CloseOkMethod CloseOk
  | OpenChannelMethod OpenChannel
  | OpenChannelOkMethod OpenChannelOk
  | FlowChannelMethod FlowChannel
  | FlowChannelOkMethod FlowChannelOk
  | CloseChannelMethod CloseChannel
  | CloseChannelOkMethod CloseChannelOk
  | DeclareExchangeMethod DeclareExchange
  | DeclareExchangeOkMethod DeclareExchangeOk
  | DeleteExchangeMethod DeleteExchange
  | DeleteExchangeOkMethod DeleteExchangeOk
  | DeclareQueueMethod DeclareQueue
  | DeclareQueueOkMethod DeclareQueueOk
  | BindQueueMethod BindQueue
  | BindQueueOkMethod BindQueueOk
  | UnbindQueueMethod UnbindQueue
  | UnbindQueueOkMethod UnbindQueueOk
  | PurgeQueueMethod PurgeQueue
  | PurgeQueueOkMethod PurgeQueueOk
  | DeleteQueueMethod DeleteQueue
  | DeleteQueueOkMethod DeleteQueueOk
  | QosMethod Qos
  | QosOkMethod QosOk
  | ConsumeMethod Consume
  | ConsumeOkMethod ConsumeOk
  | CancelMethod Cancel
  | CancelOkMethod CancelOk
  | PublishMethod Publish
  | ReturnMethod Return
  | DeliverMethod Deliver
  | GetQueueMethod GetQueue
  | GetQueueOkMethod GetQueueOk
  | GetEmptyMethod GetEmpty
  | AckMethod Ack
  | RejectMethod Reject
  | RecoverAsyncMethod RecoverAsync
  | RecoverMethod Recover
  | RecoverOkMethod RecoverOk
  | SelectMethod Select
  | SelectOkMethod SelectOk
  | CommitMethod Commit
  | CommitOkMethod CommitOk
  | RollbackMethod Rollback
  | RollbackOkMethod RollbackOk
  deriving (Show)

instance Serialize Method where
  put val =
    case val of
      StartMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 10 *> put m
      StartOkMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 11 *> put m
      SecureMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 20 *> put m
      SecureOkMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 21 *> put m
      TuneMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 30 *> put m
      TuneOkMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 31 *> put m
      OpenMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 40 *> put m
      OpenOkMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 41 *> put m
      CloseMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 50 *> put m
      CloseOkMethod m ->
        Put.putWord16be 10 *> Put.putWord16be 51 *> put m
      OpenChannelMethod m ->
        Put.putWord16be 20 *> Put.putWord16be 10 *> put m
      OpenChannelOkMethod m ->
        Put.putWord16be 20 *> Put.putWord16be 11 *> put m
      FlowChannelMethod m ->
        Put.putWord16be 20 *> Put.putWord16be 20 *> put m
      FlowChannelOkMethod m ->
        Put.putWord16be 20 *> Put.putWord16be 21 *> put m
      CloseChannelMethod m ->
        Put.putWord16be 20 *> Put.putWord16be 40 *> put m
      CloseChannelOkMethod m ->
        Put.putWord16be 20 *> Put.putWord16be 41 *> put m
      DeclareExchangeMethod m ->
        Put.putWord16be 40 *> Put.putWord16be 10 *> put m
      DeclareExchangeOkMethod m ->
        Put.putWord16be 40 *> Put.putWord16be 11 *> put m
      DeleteExchangeMethod m ->
        Put.putWord16be 40 *> Put.putWord16be 20 *> put m
      DeleteExchangeOkMethod m ->
        Put.putWord16be 40 *> Put.putWord16be 21 *> put m
      DeclareQueueMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 10 *> put m
      DeclareQueueOkMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 11 *> put m
      BindQueueMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 20 *> put m
      BindQueueOkMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 21 *> put m
      UnbindQueueMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 50 *> put m
      UnbindQueueOkMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 51 *> put m
      PurgeQueueMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 30 *> put m
      PurgeQueueOkMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 31 *> put m
      DeleteQueueMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 40 *> put m
      DeleteQueueOkMethod m ->
        Put.putWord16be 50 *> Put.putWord16be 41 *> put m
      QosMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 10 *> put m
      QosOkMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 11 *> put m
      ConsumeMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 20 *> put m
      ConsumeOkMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 21 *> put m
      CancelMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 30 *> put m
      CancelOkMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 31 *> put m
      PublishMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 40 *> put m
      ReturnMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 50 *> put m
      DeliverMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 60 *> put m
      GetQueueMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 70 *> put m
      GetQueueOkMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 71 *> put m
      GetEmptyMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 72 *> put m
      AckMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 80 *> put m
      RejectMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 90 *> put m
      RecoverAsyncMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 100 *> put m
      RecoverMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 110 *> put m
      RecoverOkMethod m ->
        Put.putWord16be 60 *> Put.putWord16be 111 *> put m
      SelectMethod m ->
        Put.putWord16be 90 *> Put.putWord16be 10 *> put m
      SelectOkMethod m ->
        Put.putWord16be 90 *> Put.putWord16be 11 *> put m
      CommitMethod m ->
        Put.putWord16be 90 *> Put.putWord16be 20 *> put m
      CommitOkMethod m ->
        Put.putWord16be 90 *> Put.putWord16be 21 *> put m
      RollbackMethod m ->
        Put.putWord16be 90 *> Put.putWord16be 30 *> put m
      RollbackOkMethod m ->
        Put.putWord16be 90 *> Put.putWord16be 31 *> put m
  get = do
    cIndex <- Get.getWord16be
    mIndex <- Get.getWord16be
    case (cIndex, mIndex) of
      (10, 10) -> StartMethod <$> get
      (10, 11) -> StartOkMethod <$> get
      (10, 20) -> SecureMethod <$> get
      (10, 21) -> SecureOkMethod <$> get
      (10, 30) -> TuneMethod <$> get
      (10, 31) -> TuneOkMethod <$> get
      (10, 40) -> OpenMethod <$> get
      (10, 41) -> OpenOkMethod <$> get
      (10, 50) -> CloseMethod <$> get
      (10, 51) -> CloseOkMethod <$> get
      (20, 10) -> OpenChannelMethod <$> get
      (20, 11) -> OpenChannelOkMethod <$> get
      (20, 20) -> FlowChannelMethod <$> get
      (20, 21) -> FlowChannelOkMethod <$> get
      (20, 40) -> CloseChannelMethod <$> get
      (20, 41) -> CloseChannelOkMethod <$> get
      (40, 10) -> DeclareExchangeMethod <$> get
      (40, 11) -> DeclareExchangeOkMethod <$> get
      (40, 20) -> DeleteExchangeMethod <$> get
      (40, 21) -> DeleteExchangeOkMethod <$> get
      (50, 10) -> DeclareQueueMethod <$> get
      (50, 11) -> DeclareQueueOkMethod <$> get
      (50, 20) -> BindQueueMethod <$> get
      (50, 21) -> BindQueueOkMethod <$> get
      (50, 50) -> UnbindQueueMethod <$> get
      (50, 51) -> UnbindQueueOkMethod <$> get
      (50, 30) -> PurgeQueueMethod <$> get
      (50, 31) -> PurgeQueueOkMethod <$> get
      (50, 40) -> DeleteQueueMethod <$> get
      (50, 41) -> DeleteQueueOkMethod <$> get
      (60, 10) -> QosMethod <$> get
      (60, 11) -> QosOkMethod <$> get
      (60, 20) -> ConsumeMethod <$> get
      (60, 21) -> ConsumeOkMethod <$> get
      (60, 30) -> CancelMethod <$> get
      (60, 31) -> CancelOkMethod <$> get
      (60, 40) -> PublishMethod <$> get
      (60, 50) -> ReturnMethod <$> get
      (60, 60) -> DeliverMethod <$> get
      (60, 70) -> GetQueueMethod <$> get
      (60, 71) -> GetQueueOkMethod <$> get
      (60, 72) -> GetEmptyMethod <$> get
      (60, 80) -> AckMethod <$> get
      (60, 90) -> RejectMethod <$> get
      (60, 100) -> RecoverAsyncMethod <$> get
      (60, 110) -> RecoverMethod <$> get
      (60, 111) -> RecoverOkMethod <$> get
      (90, 10) -> SelectMethod <$> get
      (90, 11) -> SelectOkMethod <$> get
      (90, 20) -> CommitMethod <$> get
      (90, 21) -> CommitOkMethod <$> get
      (90, 30) -> RollbackMethod <$> get
      (90, 31) -> RollbackOkMethod <$> get
      other -> fail $ "Unrecognized index: " <> show other

data Start
  = Start
      { startVersionMajor :: Int8,
        startVersionMinor :: Int8,
        startServerProperties :: PeerProperties,
        startMechanisms :: Text,
        startLocales :: Text
      }
  deriving (Show)

instance Serialize Start where
  get =
    Start
      <$> Get.getInt8
      <*> Get.getInt8
      <*> get
      <*> getLongStr
      <*> getLongStr
  put val =
    put (startVersionMajor val)
      *> put (startVersionMinor val)
      *> put (startServerProperties val)
      *> putLongStr (startMechanisms val)
      *> putLongStr (startLocales val)

data StartOk
  = StartOk
      { startOkClientProperties :: PeerProperties,
        startOkMechanism :: Text,
        startOkResponse :: Text,
        startOkLocale :: Text
      }
  deriving (Show)

instance Serialize StartOk where
  get =
    StartOk
      <$> get
      <*> getShortStr
      <*> getLongStr
      <*> getShortStr
  put val =
    put (startOkClientProperties val)
      *> putShortStr (startOkMechanism val)
      *> putLongStr (startOkResponse val)
      *> putShortStr (startOkLocale val)

data Secure
  = Secure
      { secureChallenge :: Text
      }
  deriving (Show)

instance Serialize Secure where
  get =
    Secure
      <$> getLongStr
  put val =
    putLongStr (secureChallenge val)

data SecureOk
  = SecureOk
      { secureOkResponse :: Text
      }
  deriving (Show)

instance Serialize SecureOk where
  get =
    SecureOk
      <$> getLongStr
  put val =
    putLongStr (secureOkResponse val)

data Tune
  = Tune
      { tuneChannelMax :: Int16,
        tuneFrameMax :: Int32,
        tuneHeartbeat :: Int16
      }
  deriving (Show)

instance Serialize Tune where
  get =
    Tune
      <$> Get.getInt16be
      <*> Get.getInt32be
      <*> Get.getInt16be
  put val =
    Put.putInt16be (tuneChannelMax val)
      *> Put.putInt32be (tuneFrameMax val)
      *> Put.putInt16be (tuneHeartbeat val)

data TuneOk
  = TuneOk
      { tuneOkChannelMax :: Int16,
        tuneOkFrameMax :: Int32,
        tuneOkHeartbeat :: Int16
      }
  deriving (Show)

instance Serialize TuneOk where
  get =
    TuneOk
      <$> Get.getInt16be
      <*> Get.getInt32be
      <*> Get.getInt16be
  put val =
    Put.putInt16be (tuneOkChannelMax val)
      *> Put.putInt32be (tuneOkFrameMax val)
      *> Put.putInt16be (tuneOkHeartbeat val)

data Open
  = Open
      { openVirtualHost :: Path,
        openCapabilities :: Text,
        openInsist :: Bool
      }
  deriving (Show)

instance Serialize Open where
  get =
    Open
      <$> get
      <*> getShortStr
      <*> get
  put val =
    put (openVirtualHost val)
      *> putShortStr (openCapabilities val)
      *> put (openInsist val)

data OpenOk
  = OpenOk
      { openOkKnownHosts :: Text
      }
  deriving (Show)

instance Serialize OpenOk where
  get =
    OpenOk
      <$> getShortStr
  put val =
    putShortStr (openOkKnownHosts val)

data Close
  = Close
      { closeReplyCode :: ReplyCode,
        closeReplyText :: ReplyText,
        closeClassId :: ClassId,
        closeMethodId :: MethodId
      }
  deriving (Show)

instance Serialize Close where
  get =
    Close
      <$> get
      <*> get
      <*> get
      <*> get
  put val =
    put (closeReplyCode val)
      *> put (closeReplyText val)
      *> put (closeClassId val)
      *> put (closeMethodId val)

data CloseOk = CloseOk deriving (Show)

instance Serialize CloseOk where
  put _ = pure ()
  get = pure CloseOk

data OpenChannel
  = OpenChannel
      { openOutOfBand :: Text
      }
  deriving (Show)

instance Serialize OpenChannel where
  get =
    OpenChannel
      <$> getShortStr
  put val =
    putShortStr (openOutOfBand val)

data OpenChannelOk
  = OpenChannelOk
      { openChannelOkChannelId :: Text
      }
  deriving (Show)

instance Serialize OpenChannelOk where
  get =
    OpenChannelOk
      <$> getLongStr
  put val =
    putLongStr (openChannelOkChannelId val)

data FlowChannel
  = FlowChannel
      { flowActive :: Bool
      }
  deriving (Show)

instance Serialize FlowChannel where
  get =
    FlowChannel
      <$> get
  put val =
    put (flowActive val)

data FlowChannelOk
  = FlowChannelOk
      { flowOkActive :: Bool
      }
  deriving (Show)

instance Serialize FlowChannelOk where
  get =
    FlowChannelOk
      <$> get
  put val =
    put (flowOkActive val)

data CloseChannel
  = CloseChannel
      { closeReplyCode' :: ReplyCode,
        closeReplyText' :: ReplyText,
        closeClassId' :: ClassId,
        closeMethodId' :: MethodId
      }
  deriving (Show)

instance Serialize CloseChannel where
  get =
    CloseChannel
      <$> get
      <*> get
      <*> get
      <*> get
  put val =
    put (closeReplyCode' val)
      *> put (closeReplyText' val)
      *> put (closeClassId' val)
      *> put (closeMethodId' val)

data CloseChannelOk = CloseChannelOk deriving (Show)

instance Serialize CloseChannelOk where
  put _ = pure ()
  get = pure CloseChannelOk

data DeclareExchange
  = DeclareExchange
      { declareTicket :: Int16,
        declareExchange :: ExchangeName,
        declareType :: Text,
        declareFlags :: DeclareExchangeFlags,
        declareArguments :: HashMap Text Property
      }
  deriving (Show)

instance Serialize DeclareExchange where
  get =
    DeclareExchange
      <$> Get.getInt16be
      <*> get
      <*> getShortStr
      <*> get
      <*> getTable
  put val =
    Put.putInt16be (declareTicket val)
      *> put (declareExchange val)
      *> putShortStr (declareType val)
      *> put (declareFlags val)
      *> putTable (declareArguments val)

data DeclareExchangeFlags
  = DeclareExchangeFlags
      { exchangePassive :: Bool,
        exchangeDurable :: Bool,
        exchangeAutoDelete :: Bool,
        exchangeInternal :: Bool,
        exchangeNoWait :: Bool
      }
  deriving (Show)

instance Serialize DeclareExchangeFlags where
  get = do
    byte <- Get.getWord8
    pure $
      DeclareExchangeFlags
        (testBit byte 0)
        (testBit byte 1)
        (testBit byte 2)
        (testBit byte 3)
        (testBit byte 4)
  put flags =
    Put.putWord8 $ fromIntegral $
      (fromEnum (exchangePassive flags) `shiftL` 0)
        .|. (fromEnum (exchangeDurable flags) `shiftL` 1)
        .|. (fromEnum (exchangeAutoDelete flags) `shiftL` 2)
        .|. (fromEnum (exchangeInternal flags) `shiftL` 3)
        .|. (fromEnum (exchangeNoWait flags) `shiftL` 4)

data DeclareExchangeOk = DeclareExchangeOk deriving (Show)

instance Serialize DeclareExchangeOk where
  put _ = pure ()
  get = pure DeclareExchangeOk

data DeleteExchange
  = DeleteExchange
      { deleteTicket :: Int16,
        deleteExchange :: ExchangeName,
        deleteFlags :: DeleteExchangeFlags
      }
  deriving (Show)

instance Serialize DeleteExchange where
  get =
    DeleteExchange
      <$> Get.getInt16be
      <*> get
      <*> get
  put val =
    Put.putInt16be (deleteTicket val)
      *> put (deleteExchange val)
      *> put (deleteFlags val)

data DeleteExchangeFlags
  = DeleteExchangeFlags
      { deleteIfUnused :: Bool,
        deleteNoWait :: Bool
      }
  deriving (Show)

instance Serialize DeleteExchangeFlags where
  get = do
    byte <- Get.getWord8
    pure $
      DeleteExchangeFlags
        (testBit byte 0)
        (testBit byte 1)
  put flags =
    Put.putWord8 $ fromIntegral $
      (fromEnum (deleteIfUnused flags) `shiftL` 0)
        .|. (fromEnum (deleteNoWait flags) `shiftL` 1)

data DeleteExchangeOk = DeleteExchangeOk deriving (Show)

instance Serialize DeleteExchangeOk where
  put _ = pure ()
  get = pure DeleteExchangeOk

data DeclareQueue
  = DeclareQueue
      { declareTicket' :: Int16,
        declareQueue' :: QueueName,
        declareFlags' :: DeclareQueueFlags,
        declareArguments' :: HashMap Text Property
      }
  deriving (Show)

instance Serialize DeclareQueue where
  get =
    DeclareQueue
      <$> Get.getInt16be
      <*> get
      <*> get
      <*> getTable
  put val =
    Put.putInt16be (declareTicket' val)
      *> put (declareQueue' val)
      *> put (declareFlags' val)
      *> putTable (declareArguments' val)

data DeclareQueueFlags
  = DeclareQueueFlags
      { declarePassive' :: Bool,
        declareDurable' :: Bool,
        declareExclusive' :: Bool,
        declareAutoDelete' :: Bool,
        declareNoWait' :: Bool
      }
  deriving (Show)

instance Serialize DeclareQueueFlags where
  get = do
    byte <- Get.getWord8
    pure $
      DeclareQueueFlags
        (testBit byte 0)
        (testBit byte 1)
        (testBit byte 2)
        (testBit byte 3)
        (testBit byte 4)
  put flags =
    Put.putWord8 $ fromIntegral $
      (fromEnum (declarePassive' flags) `shiftL` 0)
        .|. (fromEnum (declareDurable' flags) `shiftL` 1)
        .|. (fromEnum (declareExclusive' flags) `shiftL` 2)
        .|. (fromEnum (declareAutoDelete' flags) `shiftL` 3)
        .|. (fromEnum (declareNoWait' flags) `shiftL` 4)

data DeclareQueueOk
  = DeclareQueueOk
      { declareOkQueue :: QueueName,
        declareOkMessageCount :: MessageCount,
        declareOkConsumerCount :: Int32
      }
  deriving (Show)

instance Serialize DeclareQueueOk where
  get =
    DeclareQueueOk
      <$> get
      <*> get
      <*> Get.getInt32be
  put val =
    put (declareOkQueue val)
      *> put (declareOkMessageCount val)
      *> Put.putInt32be (declareOkConsumerCount val)

data BindQueue
  = BindQueue
      { bindTicket :: Int16,
        bindQueue :: QueueName,
        bindExchange :: ExchangeName,
        bindRoutingKey :: Text,
        bindNoWait :: NoWait,
        bindArguments :: HashMap Text Property
      }
  deriving (Show)

instance Serialize BindQueue where
  get =
    BindQueue
      <$> Get.getInt16be
      <*> get
      <*> get
      <*> getShortStr
      <*> get
      <*> getTable
  put val =
    Put.putInt16be (bindTicket val)
      *> put (bindQueue val)
      *> put (bindExchange val)
      *> putShortStr (bindRoutingKey val)
      *> put (bindNoWait val)
      *> putTable (bindArguments val)

data BindQueueOk = BindQueueOk deriving (Show)

instance Serialize BindQueueOk where
  put _ = pure ()
  get = pure BindQueueOk

data UnbindQueue
  = UnbindQueue
      { unbindTicket :: Int16,
        unbindQueue :: QueueName,
        unbindExchange :: ExchangeName,
        unbindRoutingKey :: Text,
        unbindArguments :: HashMap Text Property
      }
  deriving (Show)

instance Serialize UnbindQueue where
  get =
    UnbindQueue
      <$> Get.getInt16be
      <*> get
      <*> get
      <*> getShortStr
      <*> getTable
  put val =
    Put.putInt16be (unbindTicket val)
      *> put (unbindQueue val)
      *> put (unbindExchange val)
      *> putShortStr (unbindRoutingKey val)
      *> putTable (unbindArguments val)

data UnbindQueueOk = UnbindQueueOk deriving (Show)

instance Serialize UnbindQueueOk where
  put _ = pure ()
  get = pure UnbindQueueOk

data PurgeQueue
  = PurgeQueue
      { purgeTicket :: Int16,
        purgeQueue :: QueueName,
        purgeNoWait :: NoWait
      }
  deriving (Show)

instance Serialize PurgeQueue where
  get =
    PurgeQueue
      <$> Get.getInt16be
      <*> get
      <*> get
  put val =
    Put.putInt16be (purgeTicket val)
      *> put (purgeQueue val)
      *> put (purgeNoWait val)

data PurgeQueueOk
  = PurgeQueueOk
      { purgeOkMessageCount :: MessageCount
      }
  deriving (Show)

instance Serialize PurgeQueueOk where
  get =
    PurgeQueueOk
      <$> get
  put val =
    put (purgeOkMessageCount val)

data DeleteQueue
  = DeleteQueue
      { deleteTicket' :: Int16,
        deleteQueue' :: QueueName,
        deleteFlags' :: DeleteQueueFlags
      }
  deriving (Show)

instance Serialize DeleteQueue where
  get =
    DeleteQueue
      <$> Get.getInt16be
      <*> get
      <*> get
  put val =
    Put.putInt16be (deleteTicket' val)
      *> put (deleteQueue' val)
      *> put (deleteFlags' val)

data DeleteQueueFlags
  = DeleteQueueFlags
      { deleteIfUnused' :: Bool,
        deleteIfEmpty' :: Bool,
        deleteNoWait' :: Bool
      }
  deriving (Show)

instance Serialize DeleteQueueFlags where
  get = do
    byte <- Get.getWord8
    pure $
      DeleteQueueFlags
        (testBit byte 0)
        (testBit byte 1)
        (testBit byte 2)
  put flags =
    Put.putWord8 $ fromIntegral $
      (fromEnum (deleteIfUnused' flags) `shiftL` 0)
        .|. (fromEnum (deleteIfEmpty' flags) `shiftL` 1)
        .|. (fromEnum (deleteNoWait' flags) `shiftL` 2)

data DeleteQueueOk
  = DeleteQueueOk
      { deleteOkMessageCount :: MessageCount
      }
  deriving (Show)

instance Serialize DeleteQueueOk where
  get =
    DeleteQueueOk
      <$> get
  put val =
    put (deleteOkMessageCount val)

data Qos
  = Qos
      { qosPrefetchSize :: Int32,
        qosPrefetchCount :: Int16,
        qosGlobal :: Bool
      }
  deriving (Show)

instance Serialize Qos where
  get =
    Qos
      <$> Get.getInt32be
      <*> Get.getInt16be
      <*> get
  put val =
    Put.putInt32be (qosPrefetchSize val)
      *> Put.putInt16be (qosPrefetchCount val)
      *> put (qosGlobal val)

data QosOk = QosOk deriving (Show)

instance Serialize QosOk where
  put _ = pure ()
  get = pure QosOk

data Consume
  = Consume
      { consumeTicket :: Int16,
        consumeQueue :: QueueName,
        consumeConsumerTag :: ConsumerTag,
        consumeFlags :: ConsumeFlags,
        consumeArguments :: HashMap Text Property
      }
  deriving (Show)

instance Serialize Consume where
  get =
    Consume
      <$> Get.getInt16be
      <*> get
      <*> get
      <*> get
      <*> getTable
  put val =
    Put.putInt16be (consumeTicket val)
      *> put (consumeQueue val)
      *> put (consumeConsumerTag val)
      *> put (consumeFlags val)
      *> putTable (consumeArguments val)

data ConsumeFlags
  = ConsumeFlags
      { consumeNoLocal :: Bool,
        consumeNoAck :: Bool,
        consumeExclusive :: Bool,
        consumeNoWait :: Bool
      }
  deriving (Show)

instance Serialize ConsumeFlags where
  get = do
    byte <- Get.getWord8
    pure $
      ConsumeFlags
        (testBit byte 0)
        (testBit byte 1)
        (testBit byte 2)
        (testBit byte 3)
  put flags =
    Put.putWord8 $ fromIntegral $
      (fromEnum (consumeNoLocal flags) `shiftL` 0)
        .|. (fromEnum (consumeNoAck flags) `shiftL` 1)
        .|. (fromEnum (consumeExclusive flags) `shiftL` 2)
        .|. (fromEnum (consumeNoWait flags) `shiftL` 3)

data ConsumeOk
  = ConsumeOk
      { consumeOkConsumerTag :: ConsumerTag
      }
  deriving (Show)

instance Serialize ConsumeOk where
  get =
    ConsumeOk
      <$> get
  put val =
    put (consumeOkConsumerTag val)

data Cancel
  = Cancel
      { cancelConsumerTag :: ConsumerTag,
        cancelNoWait :: NoWait
      }
  deriving (Show)

instance Serialize Cancel where
  get =
    Cancel
      <$> get
      <*> get
  put val =
    put (cancelConsumerTag val)
      *> put (cancelNoWait val)

data CancelOk
  = CancelOk
      { cancelOkConsumerTag :: ConsumerTag
      }
  deriving (Show)

instance Serialize CancelOk where
  get =
    CancelOk
      <$> get
  put val =
    put (cancelOkConsumerTag val)

data Publish
  = Publish
      { publishTicket :: Int16,
        publishExchange :: ExchangeName,
        publishRoutingKey :: Text,
        publishFlags :: PublishFlags
      }
  deriving (Show)

instance Serialize Publish where
  get =
    Publish
      <$> Get.getInt16be
      <*> get
      <*> getShortStr
      <*> get
  put val =
    Put.putInt16be (publishTicket val)
      *> put (publishExchange val)
      *> putShortStr (publishRoutingKey val)
      *> put (publishFlags val)

data PublishFlags
  = PublishFlags
      { publishMandatory :: Bool,
        publishImmediate :: Bool
      }
  deriving (Show)

instance Serialize PublishFlags where
  get = do
    byte <- Get.getWord8
    pure $
      PublishFlags
        (testBit byte 0)
        (testBit byte 1)
  put flags =
    Put.putWord8 $ fromIntegral $
      (fromEnum (publishMandatory flags) `shiftL` 0)
        .|. (fromEnum (publishImmediate flags) `shiftL` 1)

data Return
  = Return
      { returnReplyCode :: ReplyCode,
        returnReplyText :: ReplyText,
        returnExchange :: ExchangeName,
        returnRoutingKey :: Text
      }
  deriving (Show)

instance Serialize Return where
  get =
    Return
      <$> get
      <*> get
      <*> get
      <*> getShortStr
  put val =
    put (returnReplyCode val)
      *> put (returnReplyText val)
      *> put (returnExchange val)
      *> putShortStr (returnRoutingKey val)

data Deliver
  = Deliver
      { deliverConsumerTag :: ConsumerTag,
        deliverDeliveryTag :: DeliveryTag,
        deliverRedelivered :: Redelivered,
        deliverExchange :: ExchangeName,
        deliverRoutingKey :: Text
      }
  deriving (Show)

instance Serialize Deliver where
  get =
    Deliver
      <$> get
      <*> get
      <*> get
      <*> get
      <*> getShortStr
  put val =
    put (deliverConsumerTag val)
      *> put (deliverDeliveryTag val)
      *> put (deliverRedelivered val)
      *> put (deliverExchange val)
      *> putShortStr (deliverRoutingKey val)

data GetQueue
  = GetQueue
      { getTicket' :: Int16,
        getQueue' :: QueueName,
        getNoAck' :: NoAck
      }
  deriving (Show)

instance Serialize GetQueue where
  get =
    GetQueue
      <$> Get.getInt16be
      <*> get
      <*> get
  put val =
    Put.putInt16be (getTicket' val)
      *> put (getQueue' val)
      *> put (getNoAck' val)

data GetQueueOk
  = GetQueueOk
      { getOkDeliveryTag :: DeliveryTag,
        getOkRedelivered :: Redelivered,
        getOkExchange :: ExchangeName,
        getOkRoutingKey :: Text,
        getOkMessageCount :: MessageCount
      }
  deriving (Show)

instance Serialize GetQueueOk where
  get =
    GetQueueOk
      <$> get
      <*> get
      <*> get
      <*> getShortStr
      <*> get
  put val =
    put (getOkDeliveryTag val)
      *> put (getOkRedelivered val)
      *> put (getOkExchange val)
      *> putShortStr (getOkRoutingKey val)
      *> put (getOkMessageCount val)

data GetEmpty
  = GetEmpty
      { getEmptyClusterId :: Text
      }
  deriving (Show)

instance Serialize GetEmpty where
  get =
    GetEmpty
      <$> getShortStr
  put val =
    putShortStr (getEmptyClusterId val)

data Ack
  = Ack
      { ackDeliveryTag :: DeliveryTag,
        ackMultiple :: Bool
      }
  deriving (Show)

instance Serialize Ack where
  get =
    Ack
      <$> get
      <*> get
  put val =
    put (ackDeliveryTag val)
      *> put (ackMultiple val)

data Reject
  = Reject
      { rejectDeliveryTag :: DeliveryTag,
        rejectRequeue :: Bool
      }
  deriving (Show)

instance Serialize Reject where
  get =
    Reject
      <$> get
      <*> get
  put val =
    put (rejectDeliveryTag val)
      *> put (rejectRequeue val)

data RecoverAsync
  = RecoverAsync
      { recoverAsyncRequeue :: Bool
      }
  deriving (Show)

instance Serialize RecoverAsync where
  get =
    RecoverAsync
      <$> get
  put val =
    put (recoverAsyncRequeue val)

data Recover
  = Recover
      { recoverRequeue :: Bool
      }
  deriving (Show)

instance Serialize Recover where
  get =
    Recover
      <$> get
  put val =
    put (recoverRequeue val)

data RecoverOk = RecoverOk deriving (Show)

instance Serialize RecoverOk where
  put _ = pure ()
  get = pure RecoverOk

data Select = Select deriving (Show)

instance Serialize Select where
  put _ = pure ()
  get = pure Select

data SelectOk = SelectOk deriving (Show)

instance Serialize SelectOk where
  put _ = pure ()
  get = pure SelectOk

data Commit = Commit deriving (Show)

instance Serialize Commit where
  put _ = pure ()
  get = pure Commit

data CommitOk = CommitOk deriving (Show)

instance Serialize CommitOk where
  put _ = pure ()
  get = pure CommitOk

data Rollback = Rollback deriving (Show)

instance Serialize Rollback where
  put _ = pure ()
  get = pure Rollback

data RollbackOk = RollbackOk deriving (Show)

instance Serialize RollbackOk where
  put _ = pure ()
  get = pure RollbackOk

newtype ClassId = ClassId {getClassId :: Int16} deriving (Show)

instance Serialize ClassId where
  put (ClassId val) = Put.putInt16be val
  get = ClassId <$> Get.getInt16be

newtype ConsumerTag = ConsumerTag {getConsumerTag :: Text} deriving (Show, Eq, Ord)

instance Serialize ConsumerTag where
  put (ConsumerTag val) = putShortStr val
  get = ConsumerTag <$> getShortStr

newtype DeliveryTag = DeliveryTag {getDeliveryTag :: Int64} deriving (Show)

instance Serialize DeliveryTag where
  put (DeliveryTag val) = Put.putInt64be val
  get = DeliveryTag <$> Get.getInt64be

newtype ExchangeName = ExchangeName {getExchangeName :: Text} deriving (Show)

instance Serialize ExchangeName where
  put (ExchangeName val) = putShortStr val
  get = ExchangeName <$> getShortStr

newtype MethodId = MethodId {getMethodId :: Int16} deriving (Show)

instance Serialize MethodId where
  put (MethodId val) = Put.putInt16be val
  get = MethodId <$> Get.getInt16be

newtype NoAck = NoAck {getNoAck :: Bool} deriving (Show)

instance Serialize NoAck where
  put (NoAck val) = put val
  get = NoAck <$> get

newtype NoLocal = NoLocal {getNoLocal :: Bool} deriving (Show)

instance Serialize NoLocal where
  put (NoLocal val) = put val
  get = NoLocal <$> get

newtype NoWait = NoWait {getNoWait :: Bool} deriving (Show)

instance Serialize NoWait where
  put (NoWait val) = put val
  get = NoWait <$> get

newtype Path = Path {getPath :: Text} deriving (Show)

instance Serialize Path where
  put (Path val) = putShortStr val
  get = Path <$> getShortStr

newtype PeerProperties = PeerProperties {getPeerProperties :: HashMap Text Property} deriving (Show)

instance Serialize PeerProperties where
  put (PeerProperties val) = putTable val
  get = PeerProperties <$> getTable

newtype QueueName = QueueName {getQueueName :: Text} deriving (Show)

instance Serialize QueueName where
  put (QueueName val) = putShortStr val
  get = QueueName <$> getShortStr

newtype Redelivered = Redelivered {getRedelivered :: Bool} deriving (Show)

instance Serialize Redelivered where
  put (Redelivered val) = put val
  get = Redelivered <$> get

newtype MessageCount = MessageCount {getMessageCount :: Int32} deriving (Show)

instance Serialize MessageCount where
  put (MessageCount val) = Put.putInt32be val
  get = MessageCount <$> Get.getInt32be

newtype ReplyCode = ReplyCode {getReplyCode :: Int16} deriving (Show)

instance Serialize ReplyCode where
  put (ReplyCode val) = Put.putInt16be val
  get = ReplyCode <$> Get.getInt16be

newtype ReplyText = ReplyText {getReplyText :: Text} deriving (Show)

instance Serialize ReplyText where
  put (ReplyText val) = putShortStr val
  get = ReplyText <$> getShortStr

data BasicProperties
  = BasicProperties
      { contentType :: Maybe Text,
        contentEncoding :: Maybe Text,
        headers :: Maybe (HashMap Text Property),
        deliveryMode :: Maybe Int8,
        priority :: Maybe Int8,
        correlationId :: Maybe Text,
        replyTo :: Maybe Text,
        expiration :: Maybe Text,
        messageId :: Maybe Text,
        timestamp :: Maybe Int64,
        type' :: Maybe Text,
        userId :: Maybe Text,
        appId :: Maybe Text,
        clusterId :: Maybe Text
      }
  deriving (Show)

defaultProperties :: BasicProperties
defaultProperties =
  BasicProperties
    { contentType = Nothing,
      contentEncoding = Nothing,
      headers = Nothing,
      deliveryMode = Nothing,
      priority = Nothing,
      correlationId = Nothing,
      replyTo = Nothing,
      expiration = Nothing,
      messageId = Nothing,
      timestamp = Nothing,
      type' = Nothing,
      userId = Nothing,
      appId = Nothing,
      clusterId = Nothing
    }

instance Serialize BasicProperties where
  put properties = do
    Put.putWord16be flag
    traverse_ putShortStr (contentType properties)
    traverse_ putShortStr (contentEncoding properties)
    traverse_ putTable (headers properties)
    traverse_ put (deliveryMode properties)
    traverse_ put (priority properties)
    traverse_ putShortStr (correlationId properties)
    traverse_ putShortStr (replyTo properties)
    traverse_ putShortStr (expiration properties)
    traverse_ putShortStr (messageId properties)
    traverse_ Put.putInt64be (timestamp properties)
    traverse_ putShortStr (type' properties)
    traverse_ putShortStr (userId properties)
    traverse_ putShortStr (appId properties)
    traverse_ putShortStr (clusterId properties)
    where
      flag =
        fromIntegral $
          (fromEnum (isJust (contentType properties)) `shiftL` 15)
            .|. (fromEnum (isJust (contentEncoding properties)) `shiftL` 14)
            .|. (fromEnum (isJust (headers properties)) `shiftL` 13)
            .|. (fromEnum (isJust (deliveryMode properties)) `shiftL` 12)
            .|. (fromEnum (isJust (priority properties)) `shiftL` 11)
            .|. (fromEnum (isJust (correlationId properties)) `shiftL` 10)
            .|. (fromEnum (isJust (replyTo properties)) `shiftL` 9)
            .|. (fromEnum (isJust (expiration properties)) `shiftL` 8)
            .|. (fromEnum (isJust (messageId properties)) `shiftL` 7)
            .|. (fromEnum (isJust (timestamp properties)) `shiftL` 6)
            .|. (fromEnum (isJust (type' properties)) `shiftL` 5)
            .|. (fromEnum (isJust (userId properties)) `shiftL` 4)
            .|. (fromEnum (isJust (appId properties)) `shiftL` 3)
            .|. (fromEnum (isJust (clusterId properties)) `shiftL` 2)
  get = do
    flag <- Get.getWord16be
    BasicProperties
      <$> when (testBit flag 15) getShortStr
      <*> when (testBit flag 14) getShortStr
      <*> when (testBit flag 13) getTable
      <*> when (testBit flag 12) get
      <*> when (testBit flag 11) get
      <*> when (testBit flag 10) getShortStr
      <*> when (testBit flag 9) getShortStr
      <*> when (testBit flag 8) getShortStr
      <*> when (testBit flag 7) getShortStr
      <*> when (testBit flag 6) Get.getInt64be
      <*> when (testBit flag 5) getShortStr
      <*> when (testBit flag 4) getShortStr
      <*> when (testBit flag 3) getShortStr
      <*> when (testBit flag 2) getShortStr
    where
      when bool fa =
        if bool then Just <$> fa else pure Nothing
