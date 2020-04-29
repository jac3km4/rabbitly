module AMQP.DataType (DataType (..), Type (..), fromProtocol, getType) where

import AMQP.Protocol
import Control.Applicative ((<|>))
import qualified Data.Char as Char
import qualified Data.List as List
import Data.Maybe (catMaybes)

data DataType
  = DataType
      { name :: String,
        cIndex :: Int,
        mIndex :: Int,
        fields :: [(String, Type)]
      }
  deriving (Show)

data Type
  = Bit
  | I8
  | I16
  | I32
  | I64
  | ShortStr
  | LongStr
  | Timestamp
  | Table
  | Wrapper String Type
  deriving (Show)

fromProtocol :: Protocol -> [DataType]
fromProtocol (Protocol {protocolTypes, protocolClasses}) =
  concatMap dataTypes protocolClasses
  where
    dataTypes clazz =
      dataType clazz <$> classMethods clazz
    dataType (Class {classIndex}) (Method {methodName, methodIndex, methodFields}) =
      let fields = catMaybes $ fmap (uncurry (field methodName)) methodFields
       in DataType (pascalifyUpper methodName) methodIndex classIndex fields
    field methodName name type' =
      (pascalify (methodName <> "-" <> name),) <$> fieldType type'
    fieldType ref =
      List.find ((== ref) . typeName) protocolTypes >>= getType

getType :: TypeMapping -> Maybe Type
getType (TypeMapping {typeName, typeValue}) =
  match typeName <|> Wrapper (pascalifyUpper typeName) <$> match typeValue
  where
    match str =
      case str of
        "bit" -> Just Bit
        "octet" -> Just I8
        "short" -> Just I16
        "long" -> Just I32
        "longlong" -> Just I64
        "shortstr" -> Just ShortStr
        "longstr" -> Just LongStr
        "timestamp" -> Just Timestamp
        "table" -> Just Table
        _ -> Nothing

pascalifyUpper :: String -> String
pascalifyUpper (first : rest) = Char.toUpper first : pascalify rest
pascalifyUpper [] = []

pascalify :: String -> String
pascalify ('-' : x : xs) = Char.toUpper x : pascalify xs
pascalify (x : xs) = x : pascalify xs
pascalify [] = []
