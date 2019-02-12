module MoneySpent 
(
    main
    Prices (..)
    Items (..)
)
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.Show as TS
import qualified Data.Vector as V

import qualified Anaylze.RFrame as F
import qualified Anaylze.Common as C
import qualified Anaylze.CSV as CSV

lookupFilter :: (Data k) => (k -> Bool) -> Vector k -> HashMap k Int -> Int
lookupFilter pred keys lkup = HM.lookup (head $ V.filter pred keys) lkup

removeLegalFees :: (Data v, MonadThrow m) => 
    RFrame T.Text v -> RFrame T.Text v -> [RFrame T.Text v] 
removeLegalFees prices items = 
    map (F.fliter (\keys lkup i values -> not $
        (T.isInfixOf (T.pack "legal fees") $ values !! 
            lookupFilter (T.isInfixOf (T.pack "item")) keys lkup))) 
        [prices, items]

groupBy :: (Data v, MonadThrow m) => 
    RFrame T.Text v -> T.Text -> m [RFrame T.Text v]
groupBy items tCol = V.mapM (\item -> F.filter (\keys lkup i values -> 
    item == values !! lookupFilter tCol keys lkup) items) 
       $ col tCol items

splitByItemBought :: (Data v, MonadThrow m) => 
    RFrame T.Text v -> m [RFrame T.Text v]
splitByItemBought items = groupBy items (T.pack "item-bought")

addPriceCol :: (Data v, MonadThrow m) => 
    [RFrame T.Text v] -> RFrame T.Text v -> m [RFrame T.Text v]
addPriceCol splitItems prices = do
        priceTag <- col (T.pack "price") prices

        return $ V.map (\(s,p) -> 
            addColumn s (T.pack "price") $ V.take 
                (length $ rframeData s / length $ rframeKeys s) 
                    $ V.fromList $ repeat p)
            $ V.zip splitItems priceTag

splitByPerson :: (Data v, MonadThrow m) =>
    RFrame T.Text v -> m [RFrame T.Text v]
splitByPerson items = groupBy items (T.pack "person") 

merge :: (Data v, MonadThrow m) => 
    m [R.Frame T.Text v] -> m (R.Frame T.Text v)
merge = foldM (appendRows) F.empty 

filterDates :: (Data v) =>
   R.Frame T.Text v -> R.Frame T.Text v 
filterDates = 
    F.filter (keys, lkup, i values -> 
        6 > (TR.read $ values !! lookupFilter (T.pack "date") keys lkup) :: Int)

totalPrice keys values = (TR.read $ values !!
        lookupFilter (T.pack "units") keys (C.makeLookup keys)) :: Double  * 
    (TR.read $ values !! 
        lookupFilter (T.pack "price") keys (C.makeLookup keys)) :: Double
 
accumSumCol :: (Data v) =>
    RFrame T.Text v -> V.Vector T.Text
accumSumCol items = 
    V.map (TS.show) $ V.foldl (\acc values -> snoc acc $ (last acc) +
        totalPrice keys values) V.empty (rframeData items)
   where keys = rframeKeys items

mean :: (Data v) => RFrame T.Text v -> Double
mean items = 
    V.foldl (\acc values -> acc + totalPrice keys values) 
        (0::Double) (rframeData items) / (V.length $ rframeData items)
    where keys = rframeKeys items 


main = do
    -- Load Csv files
    prices <- CSV.loadCsvFileWithHeader "./data/prices.csv" 
    items <- CSV.loadCsvFileWithHeader "./data/items.csv"

    -- remove Legal fees
    let [pricesWithoutL, itemsWithoutL] = removeLegalFees prices items

    -- merge price and purchase data 
    let splitItems = splitByItemBought itemsWithoutL
    priceItems <- merge $ addPriceCol splitItems pricesWithoutL

    -- filter dates, group by people 
    -- get accumlative sum and append it to the groups, then merge them
    finalItems <- merge $ 
        map (\person -> addColumn person (T.pack "accsum") accumSumCol)
        $ splitByPerson $ filterDates priceItems

    --show mean 
    putStrLn $ show $ mean finalItems
