{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts  #-}

module Types where 


data SourceInfo = SourceInfo {
    line :: Integer
  , fileName :: String 
} deriving (Show, Eq)


data Tag = Tag {
    json :: String 
} deriving (Show, Eq)


data Field = Field {
    fname :: String 
  , fkind :: String 
  , fdesc :: String 
  , ftag :: Tag 
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

