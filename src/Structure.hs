module Structure where 

import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO
import Control.Monad
import Data.List 

import Types
import PCombinators


p_comment :: Parser String 
p_comment = do 
    spaces
    string "//"
    comment <-  many (noneOf "\n")
    newline
    return comment 


p_comments :: Parser String 
p_comments = do 
    comments <- many $ try p_comment 
    return $ concat (intersperse "\n" comments)


p_non_empty_string :: Parser String 
p_non_empty_string = do
    spaces
    val <- many ( noneOf " \n")
    return val 


p_tag :: Parser Tag 
p_tag = do 
    spaces 
    key <- many (noneOf ":")
    string ":\""
    value <- many (noneOf "\"")
    char '"'
    spaces
    return Tag {tagKey=key, tagValue=value}


p_tags :: Parser Tags 
p_tags = do 
    spaces 
    char '`'
    tags <- manyTill p_tag (char '`')
    return Tags {
                    jsonTag = extractTag "json" tags
                  , bsonTag = extractTag "bson" tags
                  , bindingTag = extractTag "binding" tags
                  , formTag = extractTag "form" tags 
                }


p_field :: Parser Field 
p_field = do 
    comment <- p_comments
    name <- p_non_empty_string 
    kind <- p_non_empty_string 
    tags <- p_tags 
    many (noneOf "\n")
    newline
    return Field{fname= name, fdesc=comment, fkind=kind, ftag=tags, finfo=SourceInfo{line=0, fileName=""}}


p_struct :: Parser Struct 
p_struct = do 
    comment <- p_comments 
    spaces 
    string "type "
    spaces
    name <- p_non_empty_string 
    spaces 
    string "struct"
    spaces 
    char '{'
    newline
    fields <- manyTill p_field (char '}')
    return Struct {sname= name, sdesc= comment, sfields= fields, sinfo=SourceInfo{line=0, fileName=""}}


