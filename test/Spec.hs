module Main where 

import Test.Hspec

import TStructure

main :: IO ()
main = do
        -- hspec $ spec_p_comments
        -- hspec $ spec_p_non_empty_string
        -- hspec $ spec_p_tag
        -- hspec $ spec_p_field
        -- hspec $ spec_p_fields
        hspec $ spec_p_struct
