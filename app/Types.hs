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
  , timestamp :: Int
  , name      :: Text
  , desc      :: Text
} deriving (Generic, Show, Eq)
instance FromJSON Order where
  parseJSON (Object o) = Order
    <$> pure ""
    <*> pure (-1)
    <*> o .: "user_name"
    <*> o .: "text"
instance Ord Order where
  Order{timestamp = ts1} `compare` Order{timestamp = ts2}
    = ts1 `compare` ts2


data OrderList = OrderList { unOrderList :: [Order] }
instance ToJSON OrderList where
  toJSON OrderList{unOrderList = ol} =
    object ["text" .= String list]
    where
      list = T.intercalate "\n" $ map (\Order{name, desc} -> name <> ": " <> desc) ol

instance FromAttrs Order where
  parseAttrs hm = Order
    <$> parseStringAttr "datestamp" hm
    <*> parseNumberAttr "timestamp" hm
    <*> parseStringAttr "name"      hm
    <*> parseStringAttr "desc"      hm

instance ToAttrs Order where
  toAttrs Order{datestamp, timestamp, name, desc} = [
                    ("datestamp", stringAttr datestamp)
                  , ("timestamp", numberAttr timestamp)
                  , ("name",      stringAttr name)
                  , ("desc",      stringAttr desc)
                  ]


