module PCombinators where 

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Data.List 


spaces :: Parser () 
spaces = do 
        many (char ' ')
        return ()

newlineWithPreSpace :: Parser () 
newlineWithPreSpace = do 
                    many (noneOf "\n")
                    try newline
                    return ()    


