module AMQP.Protocol where

import Control.Monad ((>=>))
import qualified Data.ByteString.Char8 as BC
import qualified Data.List as List
import Data.Maybe (catMaybes)
import qualified Xeno.DOM as DOM

data Protocol
  = Protocol
      { protocolClasses :: [Class],
        protocolReplies :: [Reply],
        protocolTypes :: [TypeMapping]
      }
  deriving (Show)

data Class
  = Class
      { className :: String,
        classIndex :: Int,
        classMethods :: [Method]
      }
  deriving (Show)

data Method
  = Method
      { methodName :: String,
        methodIndex :: Int,
        methodFields :: [(String, String)]
      }
  deriving (Show)

data Reply
  = Reply
      { replyName :: String,
        code :: Int
      }
  deriving (Show)

data TypeMapping
  = TypeMapping
      { typeName :: String,
        typeValue :: String
      }
  deriving (Show)

fromFile :: String -> IO Protocol
fromFile =
  BC.readFile
    >=> fmap fromDOM . either (fail . show) pure . DOM.parse

fromDOM :: DOM.Node -> Protocol
fromDOM root =
  Protocol (classes root) (replies root) (domainTypes root)
  where
    classes =
      catMaybes
        . fmap clazz
        . filter ((== "class") . DOM.name)
        . DOM.children
    clazz node =
      Class
        <$> attribute "name" node
        <*> fmap read (attribute "index" node)
        <*> pure (methods node)
    methods =
      catMaybes
        . fmap method
        . filter ((== "method") . DOM.name)
        . DOM.children
    method node =
      Method
        <$> attribute "name" node
        <*> fmap read (attribute "index" node)
        <*> pure (fields node)
    fields =
      catMaybes
        . fmap field
        . filter ((== "field") . DOM.name)
        . DOM.children
    field node =
      (,)
        <$> attribute "name" node
        <*> attribute "domain" node
    domainTypes =
      catMaybes
        . fmap domainType
        . filter ((== "domain") . DOM.name)
        . DOM.children
    domainType node =
      TypeMapping
        <$> attribute "name" node
        <*> attribute "type" node
    replies =
      catMaybes
        . fmap reply
        . takeWhile ((== "constant") . DOM.name)
        . dropWhile (all ((/= "reply-success")) . attribute "name")
        . DOM.children
    reply node =
      Reply
        <$> attribute "name" node
        <*> fmap read (attribute "value" node)
    attribute name =
      fmap (BC.unpack . snd)
        . List.find ((== name) . fst)
        . DOM.attributes
