{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Control.Lens                    hiding (view, (.=))
import           Control.Monad                   (forM)
import           Data.Aeson
import           Data.Default                    (def)
import qualified Data.HashMap.Strict             as SHM
import           Data.List                       (sort)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Time.Calendar              (fromGregorian)
import           Data.Time.Clock                 (UTCTime (UTCTime),
                                                  diffUTCTime)
import           Data.Time.Format
import           Network.AWS.DynamoDB.DeleteItem
import           Network.AWS.DynamoDB.GetItem
import           Network.AWS.DynamoDB.PutItem
import           Network.AWS.DynamoDB.Query
import           Network.AWS.DynamoDB.Scan

import           Qi                              (withConfig)
import           Qi.Config.AWS.ApiGw             (ApiVerb (Delete, Get, Post))
import           Qi.Config.AWS.DDB               (DdbAttrDef (..),
                                                  DdbAttrType (..), dtpRangeKey)
import           Qi.Config.AWS.Lambda            (LambdaMemorySize (..),
                                                  lpMemorySize)
import           Qi.Config.Identifier            (DdbTableId)
import           Qi.Program.Config.Interface     (ConfigProgram, api,
                                                  apiMethodLambda, apiResource,
                                                  ddbTable)
import           Qi.Program.Lambda.Interface     (ApiLambdaProgram,
                                                  LambdaProgram,
                                                  deleteDdbRecord,
                                                  getCurrentTime, getDdbRecord,
                                                  putDdbRecord, queryDdbRecords,
                                                  say, scanDdbRecords)
import           Qi.Util                         (internalError, result,
                                                  success, withSuccess)
import           Qi.Util.ApiGw
import           Qi.Util.DDB

import           Types


{-
token=gIkuvaNzQIHg97ATvDxqgjtO
team_id=T0001
team_domain=example
channel_id=C2147483705
channel_name=test
user_id=U2147483697
user_name=Steve
command=/weather
text=94070
response_url=https://hooks.slack.com/commands/1234/5678
 - -}


-- Used the curl commands below to test-drive the endpoints (substitute your unique api stage url first):
{-
export API="https://6f55d08zc6.execute-api.us-east-1.amazonaws.com/v1"

curl -d "user_name=ababkin&text=cake" -H "Content-Type: application/x-www-form-urlencoded" -X POST "$API/orders"
curl -d "user_name=tsorokina&text=steak" -H "Content-Type: application/x-www-form-urlencoded" -X POST "$API/orders"

curl -X POST "$API/orders/today"

-}


main :: IO ()
main = withConfig config
  where
    config :: ConfigProgram ()
    config = do
      ordersTable <- ddbTable "orders" (DdbAttrDef "datestamp" S) $ def
        & dtpRangeKey ?~ DdbAttrDef "name" S

      api "garcon" >>= \garcon ->
        apiResource "orders" garcon >>= \orders -> do
          apiMethodLambda "incoming" Post
            orders def (put ordersTable) $ def
                        & lpMemorySize .~ M1024

          apiResource "today" orders >>= \todaysOrders -> do
            apiMethodLambda "list" Post
              todaysOrders def (getTodaysOrders ordersTable) $ def
                          & lpMemorySize .~ M1024

      return ()

    getTodaysOrders :: DdbTableId
                    -> ApiLambdaProgram
    getTodaysOrders ddbTableId event = do
        today <- getToday
        say $ T.concat ["getting orders put on date: '", today, "'..."]
        r <- queryDdbRecords ddbTableId keyCond (expAttrs today)
        withSuccess (r^.qrsResponseStatus) $
          result
            (internalError . ("Parsing error: " ++))
            (success . toJSON . OrderList . sort)
            $ forM (r^.qrsItems) parseAttrs
      where
        keyCond = Just $ T.concat ["datestamp = :datestamp"]
        expAttrs = SHM.singleton ":datestamp" . stringAttr



    put :: DdbTableId
        -> ApiLambdaProgram
    put ddbTableId event =
      withDeserializedBody event $ \(order :: Order) -> do
        today <- getToday
        now   <- getEpocTimestamp
        say $ T.concat ["putting record: ", T.pack $ show order, "..."]
        r <- putDdbRecord ddbTableId $ toAttrs order{
            datestamp = today
          , timestamp = now
          }
        say $ T.concat ["...done"]

        withSuccess (r^.pirsResponseStatus) $
          success $ object ["text" .= String "successfully put order"]

getToday :: LambdaProgram Text
getToday = do
  now <- getCurrentTime
  return . T.pack $ formatTime defaultTimeLocale "%D" now



getEpocTimestamp :: LambdaProgram Int
getEpocTimestamp =
  epochTime <$> getCurrentTime

epochTime :: UTCTime
          -> Int
epochTime =
  fromIntegral . floor . flip diffUTCTime epoch

epoch :: UTCTime
epoch = UTCTime (fromGregorian 1970 1 1) 0
