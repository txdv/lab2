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
  parseJsonValue "1" (JsonInt 1),
  parseJsonValue "5" (JsonInt 5),
  parseJsonValue "42" (JsonInt 42),
  parseJsonValue "123" (JsonInt 123),
  parseJsonValue "\"abc\"" (JsonString "abc")]

parseJsonValue :: String -> JsonValue -> TestTree
parseJsonValue jsonString expectedJsonValue = testCase ("Parse json string '" ++ jsonString ++"' and get " ++ (show expectedJsonValue))
  (assertEqual "should parse json string" (getValue $ apply jvalue jsonString) expectedJsonValue)
