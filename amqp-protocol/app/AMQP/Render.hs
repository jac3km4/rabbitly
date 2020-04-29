module AMQP.Render where

import AMQP.DataType
import Data.List (intercalate)
import Text.Printf

renderAdt :: [DataType] -> String
renderAdt types =
  printf
    "data Method\n\
    \  = %s\n\n\
    \instance Persist Method where\n\
    \  put val =\n    case val of\n      %s\n\
    \  get = do\n\
    \    cIndex <- P.getBE :: Get Word16\n\
    \    mIndex <- P.getBE :: Get Word16\n\
    \    case (cIndex, mIndex) of\n      %s\n\
    \      other -> fail $ \"Unrecognized index: \" <> show other\n\n"
    (intercalate "\n  | " (fmap member types))
    (intercalate "\n      " (fmap casePut types))
    (intercalate "\n      " (fmap caseGet types))
  where
    member (DataType {name}) =
      printf "%sMethod %s" name name
    casePut (DataType {name, cIndex, mIndex}) =
      printf
        "%sMethod m ->\n        P.putBE (%i :: Word16) *> P.putBE (%i :: Word16) *> P.put m"
        name
        cIndex
        mIndex
    caseGet (DataType {name, cIndex, mIndex}) =
      printf "(%i, %i) -> %sMethod <$> P.get" cIndex mIndex name

renderDefinition :: Type -> Maybe String
renderDefinition (Wrapper name inner) =
  Just $
    printf
      "newtype %s = %s { get%s :: %s }\n\n\
      \instance Persist %s where\n\
      \  put (%s val) = %s val\n\
      \  get = %s <$> %s\n"
      name
      name
      name
      (renderType inner)
      name
      name
      (renderPut inner)
      name
      (renderGet inner)
renderDefinition _ = Nothing

renderDataType :: DataType -> String
renderDataType (DataType {name, fields})
  | null fields =
    printf
      "data %s = %s\n\n\
      \instance Persist %s where\n\
      \  put _ = pure ()\n\
      \  get = pure %s\n"
      name
      name
      name
      name
  | otherwise =
    printf
      "data %s = %s\n  { %s\n  }\n\n\
      \instance Persist %s where\n\
      \  get = %s\n\
      \  put val = %s\n"
      name
      name
      (intercalate "\n  , " (fmap renderDataField fields))
      name
      renderDecoder
      renderEncoder
  where
    renderDataField (field, type') =
      printf "%s :: %s" field (renderType type')
    renderDecoder :: String
    renderDecoder =
      let ops = intercalate "\n    <*> " (fmap (renderGet . snd) fields)
       in printf "%s\n    <$> %s" name ops
    renderEncoder :: String
    renderEncoder =
      let op (field, type') = printf "%s (%s val)" (renderPut type') field
          ops = intercalate " *>\n    " (fmap op fields)
       in printf "\n    %s" ops

renderType :: Type -> String
renderType Bit = "Bool"
renderType I8 = "Int8"
renderType I16 = "Int16"
renderType I32 = "Int32"
renderType I64 = "Int64"
renderType ShortStr = "Text"
renderType LongStr = "Text"
renderType Timestamp = "Int64"
renderType Table = "HashMap Text Property"
renderType (Wrapper name _) = name

renderGet :: Type -> String
renderGet Bit = "P.get"
renderGet I8 = "P.get"
renderGet I16 = "P.getBE"
renderGet I32 = "P.getBE"
renderGet I64 = "P.getBE"
renderGet ShortStr = "getShortStr"
renderGet LongStr = "getLongStr"
renderGet Timestamp = "P.getBE"
renderGet Table = "getTable"
renderGet (Wrapper _ _) = "P.get"

renderPut :: Type -> String
renderPut Bit = "P.put"
renderPut I8 = "P.put"
renderPut I16 = "P.putBE"
renderPut I32 = "P.putBE"
renderPut I64 = "P.putBE"
renderPut ShortStr = "putShortStr"
renderPut LongStr = "putLongStr"
renderPut Timestamp = "P.putBE"
renderPut Table = "putTable"
renderPut (Wrapper _ _) = "P.put"
