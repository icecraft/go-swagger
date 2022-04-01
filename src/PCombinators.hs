module PCombinators where 

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.List 


spaces :: Parser () 
spaces = do 
        many (char ' ')
        return ()

p_nothing :: Parser ()
p_nothing = return ()


p_nothing_s :: Parser String
p_nothing_s = return ""


newlineWithPreSpace :: Parser () 
newlineWithPreSpace = do 
                    many (noneOf "\n")
                    try newline
                    return ()    


