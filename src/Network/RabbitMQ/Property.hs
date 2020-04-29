{-# LANGUAGE StrictData #-}

module Network.RabbitMQ.Property where

import qualified Data.ByteString as BS
import Data.Foldable (traverse_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Serialize (Get, PutM, Serialize (..))
import qualified Data.Serialize.Get as Get
import Data.Serialize.IEEE754
import qualified Data.Serialize.Put as Put
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word64, Word8)

data Property
  = PropBool Bool
  | PropI8 Int8
  | PropU8 Word8
  | PropI16 Int16
  | PropU16 Word16
  | PropI32 Int32
  | PropU32 Word32
  | PropI64 Int64
  | PropU64 Word64
  | PropFloat Float
  | PropDouble Double
  | PropDecimal Word8 Word32
  | PropShortStr Text
  | PropLongStr Text
  | PropTimestamp Word64
  | PropArray [Property]
  | PropTable (HashMap Text Property)
  | PropVoid
  deriving (Show, Eq)

instance Serialize Property where
  put fa =
    case fa of
      PropBool b -> put 't' *> put b
      PropI8 i -> put 'b' *> put i
      PropU8 i -> put 'B' *> put i
      PropI16 i -> put 'U' *> Put.putInt16be i
      PropU16 i -> put 'u' *> Put.putWord16be i
      PropI32 i -> put 'I' *> Put.putInt32be i
      PropU32 i -> put 'i' *> Put.putWord32be i
      PropI64 i -> put 'L' *> Put.putInt64be i
      PropU64 i -> put 'l' *> Put.putWord64be i
      PropFloat f -> put 'f' *> putFloat32be f
      PropDouble d -> put 'd' *> putFloat64be d
      PropDecimal scale val -> put 'D' *> put scale *> Put.putWord32be val
      PropShortStr str -> put 's' *> putShortStr str
      PropLongStr str -> put 'S' *> putLongStr str
      PropArray arr -> put 'A' *> putArray arr
      PropTimestamp ts -> put 'T' *> Put.putWord64be ts
      PropTable t -> put 'F' *> putTable t
      PropVoid -> put 'V'
  get = do
    c <- get
    case c of
      't' -> PropBool <$> get
      'b' -> PropI8 <$> get
      'B' -> PropU8 <$> get
      'U' -> PropI16 <$> Get.getInt16be
      'u' -> PropU16 <$> Get.getWord16be
      'I' -> PropI32 <$> Get.getInt32be
      'i' -> PropU32 <$> Get.getWord32be
      'L' -> PropI64 <$> Get.getInt64be
      'l' -> PropU64 <$> Get.getWord64be
      'f' -> PropFloat <$> getFloat32be
      'd' -> PropDouble <$> getFloat64be
      'D' -> PropDecimal <$> get <*> Get.getWord32be
      's' -> PropShortStr <$> getShortStr
      'S' -> PropLongStr <$> getLongStr
      'A' -> PropArray <$> getArray
      'T' -> PropTimestamp <$> Get.getWord64be
      'F' -> PropTable <$> getTable
      'V' -> pure PropVoid
      other -> fail $ "Unexpected property type: " <> show other

getLongStr :: Get Text
getLongStr = do
  len <- Get.getWord32be
  bytes <- Get.getBytes $ fromIntegral len
  pure $ TE.decodeUtf8 bytes

putLongStr :: Text -> PutM ()
putLongStr str =
  let bs = TE.encodeUtf8 str
   in Put.putWord32be (fromIntegral (BS.length bs)) *> Put.putByteString bs

getShortStr :: Get Text
getShortStr = do
  len <- Get.getWord8
  bytes <- Get.getBytes $ fromIntegral len
  pure $ TE.decodeUtf8 bytes

putShortStr :: Text -> PutM ()
putShortStr str =
  let bs = TE.encodeUtf8 str
   in Put.putWord8 (fromIntegral (BS.length bs)) *> Put.putByteString bs

getTable :: Get (HashMap Text Property)
getTable = do
  len <- Get.getWord32be
  bytes <- Get.getBytes $ fromIntegral len
  props <- either fail pure $ Get.runGet go bytes
  pure $ Map.fromList props
  where
    go = untilExhausted $ (,) <$> getShortStr <*> get

putTable :: HashMap Text Property -> PutM ()
putTable props =
  let bytes = Put.runPut $ traverse_ (uncurry field) $ Map.toList props
      len = fromIntegral $ BS.length bytes
   in Put.putWord32be len *> Put.putByteString bytes
  where
    field k v = putShortStr k *> put v

getArray :: Get [Property]
getArray = do
  len <- Get.getWord32be
  bytes <- Get.getBytes $ fromIntegral len
  either fail pure $ Get.runGet (untilExhausted get) bytes

putArray :: [Property] -> PutM ()
putArray arr =
  let bytes = Put.runPut $ traverse_ put arr
      len = fromIntegral $ BS.length bytes
   in Put.putWord32be len *> Put.putByteString bytes

untilExhausted :: Get a -> Get [a]
untilExhausted fa = do
  remaining <- Get.remaining
  if remaining > 0
    then (:) <$> fa <*> untilExhausted fa
    else pure []
