module TStructure where 

import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.Parsec
import Text.ParserCombinators.Parsec
import Structure
import Types


spec_p_comments :: Spec 
spec_p_comments = do 
  describe "comments" $ do
    it "[p] parse one line" $ do
        parse p_comment "" "// one line comment\n" `shouldParse` " one line comment"
    it "[p] parse on lines2" $ do 
        parse p_comments "" "// one line\n two line\n" `shouldParse` " one line"
    it "[p] parse mutile lines" $ do 
        parse p_comments "" "// one line\n// two line\n" `shouldParse` " one line\n two line"


spec_p_non_empty_string :: Spec 
spec_p_non_empty_string = do 
  describe "non_empty_string" $ do 
    it "[p] one_string" $ do 
      parse p_non_empty_string "" " abc " `shouldParse` "abc"
    it "[p] one_string" $ do 
      parse p_non_empty_string "" " abc efg \n " `shouldParse` "abc"
    it "[p] multiple_string" $ do 
        case parse (manyTill p_non_empty_string (char '\n') ) "" " abc efg\n " of 
          Left e -> expectationFailure $ "expected: " ++ "name" ++ "\nbut parsing failed with error:\n" ++ show e
          Right val -> shouldBe val ["abc", "efg"]


spec_p_tag :: Spec
spec_p_tag = do
    describe "tag" $ do 
      it "[p] just json tag" $ do 
        case parse p_tag "" "json:\"name\"" of 
          Left e -> expectationFailure $ "expected: " ++ "name" ++ "\nbut parsing failed with error:\n" ++ show e
          Right val -> shouldBe val Tag{tagValue="name", tagKey="json"}


spec_p_tags :: Spec
spec_p_tags = do
    describe "tags" $ do 
      it "[p] just json tag" $ do 
        case parse p_tags "" " ` json:\"name\" bson:\"name\" `" of 
          Left e -> expectationFailure $ "expected: " ++ "name" ++ "\nbut parsing failed with error:\n" ++ show e
          Right val -> shouldBe val Tags{jsonTag="name", bsonTag="name", formTag="", bindingTag=""}


spec_p_field :: Spec 
spec_p_field = do 
  describe "struct field" $ do 
    it "[p] field with last comment " $ do 
      case parse p_field " " " //just name \n Name string `json:\"name\"` \n extra_tail" of 
        Left e -> expectationFailure $ "parsing failed with error:\n" ++ show e
        Right val -> shouldBe val Field{
                                        fname="Name", 
                                        fkind="string", 
                                        fdesc="just name ", 
                                        ftag=Tags{jsonTag="name", bsonTag="", formTag="", bindingTag=""}, 
                                        finfo=SourceInfo{line=0, fileName=""}
                                      }


spec_p_struct :: Spec 
spec_p_struct = do 
  describe "struct" $ do 
    it "[p] struct1" $ do 
      case parse p_struct " " "//1for sth \n//2for sth \n type User struct {\n //just id \n ID int64 `json:\"id\"` \n}\n "  of 
        Left e -> expectationFailure $ "parsing failed with error:\n" ++ show e
        Right val -> shouldBe val Struct{
                                        sname="User",  
                                        sdesc="1for sth \n2for sth ", 
                                        sfields= [Field{
                                                        fname="ID", 
                                                        fkind="int64", 
                                                        fdesc="just id ", 
                                                        ftag=Tags{jsonTag="id", bsonTag="", formTag="", bindingTag=""}, 
                                                        finfo=SourceInfo{line=0, fileName=""} }],
                                        sinfo=SourceInfo{line=0, fileName=""}}


