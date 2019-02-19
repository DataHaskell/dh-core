module MoneyFixtures where

import qualified Data.Text            as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector          as V

import           Data.Text            (Text)
import           Data.Vector          (Vector)
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)

noLegalFees :: LBS.ByteString 
noLegalFees = encodeUtf8 (T.pack ("date,item-bought,person,units\n" `mappend`
    "6,motorbike,bob,2\n" `mappend` "6,computer,alice,2"))

datesFiltered :: LBS.ByteString
datesFiltered = encodeUtf8 
    (T.pack $ "date,item-bought,person,units\n" `mappend`
        "5,legal fees,bob,3\n")

personsGrouped :: [LBS.ByteString]
personsGrouped = map encodeUtf8 persons
    where persons = [T.pack ("date,item-bought,person,units\n" `mappend` 
        "legal fees,bob,3\n" `mappend` "5,motorbike,bob,2\n" ), 
                T.pack "date,item-bought,person,units\n" `mappend` 
            "6,computer,alice,2"]

withpriceCol :: [LBS.ByteString]
withpriceCol = map encodeUtf8 persons
    where persons = [T.pack ("date,item-bought,person,units,price\n" `mappend` 
        "legal fees,bob,3,300\n" `mappend` "5,motorbike,bob,2,200\n" ), 
                T.pack "date,item-bought,person,units\n" `mappend` 
            "6,computer,alice,2,100"]

accumSum :: Vector Text
accumSum = V.fromList $ ["900", "1300"]

totalMean :: Double 
totalMean = 500

examplePriceCsv :: Text 
examplePriceCsv = "item,price\n" `mappend` "legal fees,300" `mappend` 
    "motorbike,200" `mappend` "computer,100"

exampleUnitsCsv :: Text  
exampleUnitsCsv = "date,item-bought,person,units\n" `mappend` 
        "5,legal fees,bob,3\n" `mappend` "6,computer,alice,2" `mappend` 
            "6,motorbike,bob,2"

