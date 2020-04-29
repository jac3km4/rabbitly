# rabbitly

This is Haskell RabbitMQ client library built with [Streamly](https://hackage.haskell.org/package/streamly).

The main features of this library are:
- declarative DSL for defining consumers as folds on streams of messages
- built with streams from the bottom up
- implements the entire AMQP 0-9-1 protocol in pure Haskell

# examples

### basic subscribtion
```haskell
import qualified Network.RabbitMQ as RMQ
import qualified Streamly.Data.Fold as FL
import qualified Streamly.Prelude as S

main :: IO ()
main = do
  publisher <- RMQ.chanPublisher
  S.drain $ RMQ.runPubSubS address credentials publisher subscriptions
  where
    address = RMQ.Address (127, 0, 0, 1) 5672
    credentials = RMQ.Credentials "guest" "guest"

    subscriptions =
      [ RMQ.Subscription (RMQ.QueueName "test") (RMQ.ConsumeFlags False True False False) $
          FL.drainBy print
      ]
```

### basic publisher
```haskell
import qualified Network.RabbitMQ as RMQ
import qualified Streamly.Prelude as S

main :: IO ()
main = do
  publisher <- RMQ.chanPublisher
  RMQ.publish publisher $
    RMQ.Envelope (RMQ.ExchangeName "test") (RMQ.RoutingKey "#") "msg" RMQ.defaultProperties
  S.drain $ RMQ.runPubSubS address credentials publisher []
  where
    address = RMQ.Address (127, 0, 0, 1) 5672
    credentials = RMQ.Credentials "guest" "guest"
```
