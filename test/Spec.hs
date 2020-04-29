import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as Builder
import qualified Data.HashMap.Strict as Map
import qualified Data.Serialize as Serialize
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified Network.RabbitMQ.Property as Property
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.Hedgehog (testProperty)

main :: IO ()
main =
  defaultMain $
    testGroup
      "tests"
      [testProperty "property roundtrip" propertyRoundtrip]

genProperty :: Gen Property.Property
genProperty =
  Gen.choice
    [ Property.PropBool <$> Gen.bool,
      Property.PropI8 <$> Gen.int8 Range.exponentialBounded,
      Property.PropI16 <$> Gen.int16 Range.exponentialBounded,
      Property.PropU16 <$> Gen.integral Range.exponentialBounded,
      Property.PropI32 <$> Gen.int32 Range.exponentialBounded,
      Property.PropU32 <$> Gen.integral Range.exponentialBounded,
      Property.PropI64 <$> Gen.int64 Range.exponentialBounded,
      Property.PropU64 <$> Gen.integral Range.exponentialBounded,
      Property.PropFloat <$> Gen.float (Range.exponentialFloat 0 1000),
      Property.PropDouble <$> Gen.realFloat (Range.exponentialFloat 0 1000),
      Property.PropDecimal <$> Gen.word8 Range.exponentialBounded <*> Gen.word32 Range.constantBounded,
      Property.PropShortStr <$> Gen.text (Range.linear 0 10) Gen.alpha,
      Property.PropLongStr <$> Gen.text (Range.linear 0 10) Gen.alpha,
      Property.PropTimestamp <$> Gen.word64 Range.exponentialBounded,
      Property.PropArray <$> Gen.list (Range.linear 0 10) genProperty,
      Property.PropTable . Map.fromList <$> genProperties,
      pure Property.PropVoid
    ]
  where
    genProperties =
      Gen.list (Range.linear 0 10) $
        (,) <$> Gen.text (Range.linear 0 10) Gen.alpha <*> genProperty

propertyRoundtrip :: Property
propertyRoundtrip =
  property $ do
    val <- forAll genProperty
    let encoded = Serialize.encode val
    Serialize.decode encoded === Right val
