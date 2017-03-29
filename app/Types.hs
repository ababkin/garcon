{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid          ((<>))
import           Data.String          (IsString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics
import           Network.AWS.DynamoDB (AttributeValue)

import           Qi.Util.DDB


data Order = Order {
    datestamp :: Text
  , name      :: Text
  , desc      :: Text
} deriving (Generic, Show)
instance FromJSON Order where
  parseJSON (Object o) = Order
    <$> pure ""
    <*> o .: "user_name"
    <*> o .: "text"

data OrderList = OrderList { unOrderList :: [Order] }
instance ToJSON OrderList where
  toJSON OrderList{unOrderList = ol} =
    object ["text" .= String list]
    where
      list = T.intercalate "\n" $ map (\Order{name, desc} -> name <> ": " <> desc) ol

instance FromAttrs Order where
  parseAttrs hm = Order
    <$> parseStringAttr "datestamp"   hm
    <*> parseStringAttr "name"  hm
    <*> parseStringAttr "desc"  hm

instance ToAttrs Order where
  toAttrs Order{datestamp, name, desc} = [
                    ("datestamp",   stringAttr datestamp)
                  , ("name",  stringAttr name)
                  , ("desc",  stringAttr desc)
                  ]


