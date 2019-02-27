{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase #-}

{- |
Mushroom data set

UCI ML Repository link <https://archive.ics.uci.edu/ml/datasets/mushroom>

This data set includes descriptions of hypothetical samples corresponding to 23 species of gilled mushrooms in the Agaricus and Lepiota Family. Each species is identified as definitely edible, definitely poisonous, or of unknown edibility and not recommended. This latter class was combined with the poisonous one.


Attribute Information:

All the attributes are discrete (categorical), and attribute 11 ("stalk-root") lacks some entries. The 'classification' attribute has been mapped to a Boolean value (where edible = True).

0. classification: poisonous=p, edible=e

1. cap-shape: bell=b,conical=c,convex=x,flat=f, knobbed=k,sunken=s
2. cap-surface: fibrous=f,grooves=g,scaly=y,smooth=s
3. cap-color: brown=n,buff=b,cinnamon=c,gray=g,green=r, pink=p,purple=u,red=e,white=w,yellow=y
4. bruises?: bruises=t,no=f
5. odor: almond=a,anise=l,creosote=c,fishy=y,foul=f, musty=m,none=n,pungent=p,spicy=s
6. gill-attachment: attached=a,descending=d,free=f,notched=n
7. gill-spacing: close=c,crowded=w,distant=d
8. gill-size: broad=b,narrow=n
9. gill-color: black=k,brown=n,buff=b,chocolate=h,gray=g, green=r,orange=o,pink=p,purple=u,red=e, white=w,yellow=y
10. stalk-shape: enlarging=e,tapering=t
11. stalk-root: bulbous=b,club=c,cup=u,equal=e, rhizomorphs=z,rooted=r,missing=?
12. stalk-surface-above-ring: fibrous=f,scaly=y,silky=k,smooth=s
13. stalk-surface-below-ring: fibrous=f,scaly=y,silky=k,smooth=s
14. stalk-color-above-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
15. stalk-color-below-ring: brown=n,buff=b,cinnamon=c,gray=g,orange=o, pink=p,red=e,white=w,yellow=y
16. veil-type: partial=p,universal=u
17. veil-color: brown=n,orange=o,white=w,yellow=y
18. ring-number: none=n,one=o,two=t
19. ring-type: cobwebby=c,evanescent=e,flaring=f,large=l, none=n,pendant=p,sheathing=s,zone=z
20. spore-print-color: black=k,brown=n,buff=b,chocolate=h,green=r, orange=o,purple=u,white=w,yellow=y
21. population: abundant=a,clustered=c,numerous=n, scattered=s,several=v,solitary=y
22. habitat: grasses=g,leaves=l,meadows=m,paths=p, urban=u,waste=w,woods=d

Example rows:

p,x,s,n,t,p,f,c,n,k,e,e,s,s,w,w,p,w,o,p,k,s,u

e,x,s,y,t,a,f,c,b,k,e,c,s,s,w,w,p,w,o,p,n,n,g
-}
module Numeric.Datasets.Mushroom (
    MushroomEntry(..)
  , CapShape(..), CapSurface(..), CapColor(..), Odor(..)
  , GillAttachment(..), GillSpacing(..), GillSize(..), GillColor(..), StalkShape(..)
  , StalkRoot(..), StalkSurfaceAboveRing(..), StalkSurfaceBelowRing(..)
  , StalkColorAboveRing(..), StalkColorBelowRing(..), VeilType(..), VeilColor(..)
  , RingNumber(..), RingType(..), SporePrintColor(..), Population(..), Habitat(..) 
  , mushroomDatabase) where

import Numeric.Datasets

import Data.Csv
import GHC.Generics
import Control.Applicative

data MushroomEntry = MushroomEntry {
    edible :: Bool -- ^ Is the mushroom edible?
  , capShape :: CapShape
  , capSurface :: CapSurface
  , capColor :: CapColor
  , bruises :: Bool
  , odor :: Odor
  , gillAttachment :: GillAttachment
  , gillSpacing :: GillSpacing
  , gillSize :: GillSize
  , gillColor :: GillColor
  , stalkShape :: StalkShape
  , stalkRoot :: Maybe StalkRoot
  , stalkSurfaceAboveRing :: StalkSurfaceAboveRing
  , stalkSurfaceBelowRing :: StalkSurfaceBelowRing
  , stalkColorAboveRing :: StalkColorAboveRing
  , stalkColorBelowRing :: StalkColorBelowRing
  , veilType :: VeilType
  , veilColor :: VeilColor
  , ringNumber :: RingNumber
  , ringType :: RingType
  , sporePrintColor :: SporePrintColor
  , population :: Population
  , habitat :: Habitat  } deriving (Show, Read, Generic)

instance FromRecord MushroomEntry where
  parseRecord v = MushroomEntry <$>
    (charToClassification <$> v .! 0)  <*>
    (charToCapShape <$> v .! 1)  <*>
    (charToCapSurface <$> v .! 2)  <*>
    (charToCapColor <$> v .! 3)  <*>    
    (charToBruises <$> v .! 4)  <*>
    (charToOdor <$> v .! 5)  <*>
    (charToGillAttachment <$> v .! 6)  <*>
    (charToGillSpacing <$> v .! 7)  <*>
    (charToGillSize <$> v .! 8)  <*>
    (charToGillColor <$> v .! 9)  <*>
    (charToStalkShape <$> v .! 10)  <*>
    (charToStalkRoot <$> v .! 11) <*>
    (charToSsar <$> v .! 12) <*>
    (charToSsbr <$> v .! 13) <*>
    (charToScar <$> v .! 14) <*>
    (charToScbr <$> v .! 15) <*>
    (charToVeilType <$> v .! 16) <*>
    (charToVeilColor <$> v .! 17) <*>
    (charToRingNumber <$> v .! 18) <*>
    (charToRingType <$> v .! 19) <*>
    (charToSporePrintColor <$> v .! 20) <*>
    (charToPopulation <$> v .! 21) <*>
    (charToHabitat <$> v .! 22)     

-- | Whether a mushroom sample is poisonous or edible; the classification tasks involves predicting this label.
-- data Classification = Poisonous | Edible deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToClassification :: Char -> Bool
charToClassification = \case 
  'p' -> False
  'e' -> True
  x -> error $ unwords ["Unexpected feature value :", show x]

data CapShape = Bell | Conical | Convex | Flat | Knobbed | Sunken deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToCapShape :: Char -> CapShape
charToCapShape = \case
  'b' -> Bell
  'c' -> Conical
  'x' -> Convex
  'f' -> Flat
  'k' -> Knobbed
  's' -> Sunken
  x -> error $ unwords ["Unexpected feature value :", show x]
                     
data CapSurface = CSFibrous | CSGrooves | CSScaly | CSSmooth deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToCapSurface :: Char -> CapSurface
charToCapSurface = \case
  'f' -> CSFibrous
  'g' -> CSGrooves
  'y' -> CSScaly
  's' -> CSSmooth
  x -> error $ unwords ["Unexpected feature value :", show x]  

data CapColor = CCBrown | CCBuff | CCCinnamon | CCGray | CCGreen | CCPink | CCPurple | CCRed | CCWhite | CCYellow deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)

charToCapColor :: Char -> CapColor
charToCapColor = \case
  'n' -> CCBrown
  'b' -> CCBuff
  'c' -> CCCinnamon
  'g' -> CCGray
  'r' -> CCGreen
  'p' -> CCPink
  'u' -> CCPurple
  'e' -> CCRed
  'w' -> CCWhite
  'y' -> CCYellow  
  x -> error $ unwords ["Unexpected feature value :", show x]
  
charToBruises :: Char -> Bool
charToBruises c = case c of
  't' -> True
  'f' -> False
  x -> error $ unwords ["Unexpected feature value :", show x]  

data Odor = Almond | Anise | Creosote | Fishy | Foul | Musty | None | Pungent | Spicy deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToOdor :: Char -> Odor
charToOdor = \case
  'a' -> Almond
  'l' -> Anise
  'c' -> Creosote
  'y' -> Fishy
  'f' -> Foul
  'm' -> Musty
  'n' -> None
  'p' -> Pungent
  's' -> Spicy
  x -> error $ unwords ["Unexpected feature value :", show x]  

data GillAttachment = Attached | Descending | Free | Notched deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToGillAttachment :: Char -> GillAttachment
charToGillAttachment = \case
  'a' -> Attached
  'd' -> Descending
  'f' -> Free
  'n' -> Notched
  x -> error $ unwords ["Unexpected feature value :", show x]  
  
data GillSpacing = Close | Crowded | Distant deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToGillSpacing :: Char -> GillSpacing
charToGillSpacing = \case
  'c' -> Close
  'w' -> Crowded
  'd' -> Distant
  x -> error $ unwords ["Unexpected feature value :", show x]   
  
data GillSize = Broad | Narrow deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToGillSize :: Char -> GillSize
charToGillSize = \case
  'b' -> Broad
  'n' -> Narrow
  x -> error $ unwords ["Unexpected feature value :", show x]   
  
data GillColor = GCBlack | GCBrown | GCBuff | GCChocolate | GCGray | GCGreen | GCOrange | GCPink | GCPurple | GCRed | GCWhite | GCYellow deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToGillColor :: Char -> GillColor
charToGillColor = \case
  'k' -> GCBlack
  'n' -> GCBrown
  'b' -> GCBuff
  'h' -> GCChocolate
  'g' -> GCGray
  'r' -> GCGreen
  'o' -> GCOrange
  'p' -> GCPink
  'u' -> GCPurple
  'e' -> GCRed
  'w' -> GCYellow
  x -> error $ unwords ["Unexpected feature value :", show x]   
  
data StalkShape = Enlarging | Tapering deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToStalkShape :: Char -> StalkShape
charToStalkShape = \case
  'e' -> Enlarging
  't' -> Tapering
  x -> error $ unwords ["Unexpected feature value :", show x]   

data StalkRoot = Bulbous | Club | Cup | Equal | Rhizomorphs | Rooted deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)

charToStalkRoot :: Char -> Maybe StalkRoot
charToStalkRoot c = case c of
  'b' -> Just Bulbous
  'c' -> Just Club
  'u' -> Just Cup
  'e' -> Just Equal
  'z' -> Just Rhizomorphs
  'r' -> Just Rooted
  '?' -> Nothing
  x -> error $ unwords ["Unexpected feature value :", show x]   
  
data StalkSurfaceAboveRing = SSARFibrous | SSARScaly | SSARSilky | SSARSmooth deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToSsar :: Char -> StalkSurfaceAboveRing
charToSsar = \case
  'f' -> SSARFibrous
  'y' -> SSARScaly
  'k' -> SSARSilky
  's' -> SSARSmooth
  x -> error $ unwords ["Unexpected feature value :", show x]   
  
data StalkSurfaceBelowRing = SSBRFibrous | SSBRScaly | SSBRSilky | SSBRSmooth deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToSsbr :: Char -> StalkSurfaceBelowRing
charToSsbr = \case
  'f' -> SSBRFibrous
  'y' -> SSBRScaly
  'k' -> SSBRSilky
  's' -> SSBRSmooth
  x -> error $ unwords ["Unexpected feature value :", show x]   
  
data StalkColorAboveRing = SCARBrown | SCARBuff | SCARCinnamon | SCARGray | SCAROrange | SCARPink | SCARRed | SCARWhite | SCARYellow deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToScar :: Char -> StalkColorAboveRing
charToScar = \case
  'n' -> SCARBrown
  'b' -> SCARBuff
  'c' -> SCARCinnamon
  'g' -> SCARGray
  'o' -> SCAROrange
  'p' -> SCARPink
  'e' -> SCARRed
  'w' -> SCARWhite
  'y' -> SCARYellow
  x -> error $ unwords ["Unexpected feature value :", show x]
  
data StalkColorBelowRing = SCBRBrown | SCBRBuff | SCBRCinnamon | SCBRGray | SCBROrange | SCBRPink | SCBRRed | SCBRWhite | SCBRYellow deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToScbr :: Char -> StalkColorBelowRing
charToScbr = \case
  'n' -> SCBRBrown
  'b' -> SCBRBuff
  'c' -> SCBRCinnamon
  'g' -> SCBRGray
  'o' -> SCBROrange
  'p' -> SCBRPink
  'e' -> SCBRRed
  'w' -> SCBRWhite
  'y' -> SCBRYellow
  x -> error $ unwords ["Unexpected feature value :", show x]    
  
data VeilType = Partial | Universal deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToVeilType :: Char -> VeilType
charToVeilType = \case
  'p' -> Partial
  'u' -> Universal
  x -> error $ unwords ["Unexpected feature value :", show x]    
  
data VeilColor = VCBrown | VCOrange | VCWhite | VCYellow deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToVeilColor :: Char -> VeilColor
charToVeilColor = \case
  'n' -> VCBrown
  'o' -> VCOrange
  'w' -> VCWhite
  'y' -> VCYellow
  x -> error $ unwords ["Unexpected feature value :", show x]    
  
-- | 18. ring-number: none=n,one=o,two=t
data RingNumber = RNNone | RNOne | RNTwo deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToRingNumber :: Char -> RingNumber
charToRingNumber = \case
  'n' -> RNNone
  'o' -> RNOne
  't' -> RNTwo
  x -> error $ unwords ["Unexpected feature value :", show x]    
  
data RingType = RTCobwebby | RTEvanescent | RTFlaring | RTLarge | RTNone | RTPendant | RTSheathing | RTZone deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToRingType :: Char -> RingType
charToRingType = \case
  'c' -> RTCobwebby
  'e' -> RTEvanescent
  'f' -> RTFlaring
  'l' -> RTLarge
  'n' -> RTNone
  'p' -> RTPendant
  's' -> RTSheathing
  'z' -> RTZone
  x -> error $ unwords ["Unexpected feature value :", show x]    
  
data SporePrintColor = SPCBlack | SPCBrown | SPCBuff | SPCChocolate | SPCGreen | SPCOrange | SPCPurple | SPCWhite | SPCYellow deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToSporePrintColor :: Char -> SporePrintColor
charToSporePrintColor = \case
  'k' -> SPCBlack
  'n' -> SPCBrown
  'b' -> SPCBuff
  'h' -> SPCChocolate
  'r' -> SPCGreen
  'o' -> SPCOrange
  'u' -> SPCPurple
  'w' -> SPCWhite
  'y' -> SPCYellow
  x -> error $ unwords ["Unexpected feature value :", show x]    
  
data Population = Abundant | Clustered | Numerous | Scattered | Several | Solitary deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToPopulation :: Char -> Population
charToPopulation = \case
  'a' -> Abundant
  'c' -> Clustered
  'n' -> Numerous
  's' -> Scattered
  'v' -> Several
  'y' -> Solitary
  x -> error $ unwords ["Unexpected feature value :", show x]    
  
data Habitat = Grasses | Leaves | Meadows | Paths | Urban | Waste | Woods deriving (Eq, Read, Show, Ord, Enum, Bounded, Generic)
charToHabitat :: Char -> Habitat
charToHabitat = \case
  'g' -> Grasses
  'l' -> Leaves
  'm' -> Meadows
  'p' -> Paths
  'u' -> Urban
  'w' -> Waste
  'd' -> Woods
  x -> error $ unwords ["Unexpected feature value :", show x]    


mushroomDatabase :: Dataset MushroomEntry
mushroomDatabase = csvDataset
   $ URL "https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/agaricus-lepiota.data"
