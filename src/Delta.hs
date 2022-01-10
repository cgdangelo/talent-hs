{-# LANGUAGE OverloadedStrings #-}

module Delta () where

import qualified Data.Binary as B
import Data.Bits
import qualified Data.ByteString.Lazy as L
import Data.Int (Int32)

type DeltaDecoder = (L.ByteString, [DeltaFieldDecoder])

data DeltaFieldDecoder
  = DeltaFieldDecoder
      L.ByteString -- name
      DeltaType -- type
      DeltaSignedType -- isSigned
      Int32 -- bits
      Int32 -- divisor
      (Maybe Int32) -- offset
      (Maybe Int32) -- preMultiplier
      (Maybe Int32) -- size

data DeltaType
  = DT_BYTE
  | DT_SHORT
  | DT_FLOAT
  | DT_INTEGER
  | DT_ANGLE
  | DT_TIMEWINDOW_8
  | DT_TIMEWINDOW_BIG
  | DT_STRING
  deriving (Eq, Show)

data DeltaSignedType = DT_SIGNED | DT_UNSIGNED

type DeltaFieldGetter a = B.Get (DeltaField a)

type DeltaField a = (L.ByteString, a)

initialDecoders :: [DeltaDecoder]
initialDecoders =
  [ ( "delta_description_t",
      [ DeltaFieldDecoder "flags" DT_INTEGER DT_UNSIGNED 32 1 Nothing Nothing Nothing,
        DeltaFieldDecoder "name" DT_STRING DT_UNSIGNED 8 1 Nothing Nothing Nothing,
        DeltaFieldDecoder "offset" DT_INTEGER DT_UNSIGNED 16 1 Nothing Nothing Nothing,
        DeltaFieldDecoder "size" DT_INTEGER DT_UNSIGNED 8 1 Nothing Nothing Nothing,
        DeltaFieldDecoder "bits" DT_INTEGER DT_UNSIGNED 8 1 Nothing Nothing Nothing,
        DeltaFieldDecoder "divisor" DT_INTEGER DT_UNSIGNED 32 1 Nothing Nothing Nothing,
        DeltaFieldDecoder "preMultiplier" DT_INTEGER DT_UNSIGNED 32 4000 Nothing Nothing Nothing
      ]
    )
  ]
