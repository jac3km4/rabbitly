module Main where

import qualified AMQP.DataType as DataType
import qualified AMQP.Protocol as Protocol
import qualified AMQP.Render as Render
import Data.Foldable (fold)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  protocol <- Protocol.fromFile "amqp0-9-1.xml"
  let dataTypes = DataType.fromProtocol protocol
  let basicTypes = catMaybes $ DataType.getType <$> Protocol.protocolTypes protocol
  let output =
        fold
          [ Render.renderAdt dataTypes,
            unlines $ Render.renderDataType <$> dataTypes,
            unlines $ catMaybes $ Render.renderDefinition <$> basicTypes
          ]
  writeFile "out.hs" output
