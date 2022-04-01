module Structure where 

import Text.ParserCombinators.Parsec hiding (spaces)
import System.IO
import Control.Monad
import Data.List 

import Types
import PCombinators


p_nothing :: Parser ()
p_nothing = return ()


p_nothing_s :: Parser String
p_nothing_s = return ""


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


-- 支持任意顺序的 tag
p_tag :: Parser Tag 
p_tag = do 
    spaces 
    char '`'
    spaces
    string "json:"
    jsonTag <- many (noneOf " `")
    char '`'
    return Tag {json=jsonTag}


p_field :: Parser Field 
p_field = do 
    comment <- p_comments
    name <- p_non_empty_string 
    kind <- p_non_empty_string 
    tag <- p_tag 
    many (noneOf "\n")
    newline
    return Field{fname= name, fdesc=comment, fkind=kind, ftag=tag, finfo=SourceInfo{line=0, fileName=""}}


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


