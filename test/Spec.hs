import Lib(bla)
import Parser(
  JsonValue(JsonInt, JsonString),
  jint, jstring, jvalue,
  apply, getValue)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Parser Tests" parserTests)

parserTests = [
  parseJsonInt "1" 1,
  parseJsonInt "5" 5,
  parseJsonInt "42" 42,
  parseJsonInt "123" 123,
  parseJsonString "\"abc\"" "abc"]

parseJsonInt string int = testCase ("Parse json value '" ++ string ++ "' successfully")
  (assertEqual "should parse json int" (getValue $ apply jvalue string) (JsonInt int))


parseJsonString :: String -> String -> TestTree
parseJsonString jsonString expectedString = testCase ("Parse json string '" ++ jsonString ++ "' to '" ++ expectedString ++ "'")
  (assertEqual "should parse json string" (getValue $ apply jstring jsonString) (JsonString expectedString))

tests = [ sayYoTest, add5Test, testBla ]

sayYo s = "Yo " ++ s ++ "!"
add5 x = 5 + x

sayYoTest :: TestTree
sayYoTest = testCase "Testing sayYo"
  (assertEqual "Should say Yo to Friend!" "Yo Friend!" (sayYo "Friend"))

add5Test :: TestTree
add5Test = testCase "Testing add5"
  (assertEqual "Should add 5 to get 10" 10 (add5 5))

testBla :: TestTree
testBla = testCase "bla is 5"
  (assertEqual "bla is 5" 5 bla)
