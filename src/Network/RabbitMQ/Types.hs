{-# LANGUAGE StrictData #-}

module Network.RabbitMQ.Types where

import Control.Exception (Exception)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word8)
import Network.RabbitMQ.Protocol
import Network.Socket (PortNumber)

data RabbitException
  = FormatException String
  | ProtocolException String
  | BrokerException String
  deriving (Show)

instance Exception RabbitException

data Message
  = Envelope ExchangeName RoutingKey ByteString BasicProperties
  | Ack DeliveryTag
  | Reject DeliveryTag Bool
  deriving (Show)

data Delivery
  = Delivery
      { consumerTag :: ConsumerTag,
        deliveryTag :: DeliveryTag,
        payload :: ByteString
      }
  deriving (Show)

newtype RoutingKey
  = RoutingKey
      { getRoutingKey :: Text
      }
  deriving (Show)

data Credentials
  = Credentials
      { user :: Text,
        password :: Text
      }

data Address
  = Address (Word8, Word8, Word8, Word8) PortNumber
