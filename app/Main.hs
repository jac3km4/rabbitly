module Main where

import qualified Network.RabbitMQ as RMQ
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Prelude as S

main :: IO ()
main = do
  publisher <- RMQ.chanPublisher
  RMQ.publish publisher $
    RMQ.Envelope (RMQ.ExchangeName "") (RMQ.RoutingKey "test") "msg" RMQ.defaultProperties
  S.drain $ RMQ.runPubSubS address credentials publisher subscriptions
  where
    address = RMQ.Address (127, 0, 0, 1) 5672
    credentials = RMQ.Credentials "guest" "guest"
    subscriptions =
      [ RMQ.Subscription (RMQ.QueueName "test") (RMQ.ConsumeFlags False True False False) $
          FL.drainBy print
      ]
