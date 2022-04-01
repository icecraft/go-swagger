{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts  #-}

module Types where 

import Data.List 

data SourceInfo = SourceInfo {
    line :: Integer
  , fileName :: String 
} deriving (Show, Eq)


data Tag = Tag {
    tagKey :: String 
  , tagValue :: String
} deriving (Show, Eq)


data Tags = Tags {
      jsonTag :: String 
  ,   bsonTag :: String 
  ,   bindingTag :: String 
  ,   formTag :: String 
} deriving (Show, Eq)


extractTag :: String -> [Tag] -> String 
extractTag key arr = case find (\x -> tagKey x == key) arr of 
                        Just val -> tagValue val
                        Nothing -> ""


data Field = Field {
    fname :: String 
  , fkind :: String 
  , fdesc :: String 
  , ftag :: Tags 
  , finfo :: SourceInfo 
} deriving (Show, Eq)



data Fields = Fields {
    fields :: [Field]
  , dummy :: String 
} deriving (Show, Eq)



data Struct = Struct {
    sname :: String 
  , sdesc :: String 
  , sfields :: [Field]
  , sinfo :: SourceInfo 
} deriving (Show, Eq)

