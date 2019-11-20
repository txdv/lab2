import Lib(bla)
import Parser(JsonValue(JsonInt), jvalue, apply, one, getValue)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain (testGroup "Parser Tests" parserTests)

parserTests = [
  parseJsonInt "1" 1,
  parseJsonInt "5" 5,
  parseJsonInt "42" 42,
  parseJsonInt "123" 123]

parseJsonInt string int = testCase ("Parse json value '" ++ string ++ "' successfully")
  (assertEqual "should parse json value" (getValue $ apply jvalue string) (JsonInt int))

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
