import Lib(bla)
import Parser(
  JsonValue(JsonInt, JsonString, JsonList, JsonMap),
  jvalue,
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
  parseJsonValue "\"abc\"" (JsonString "abc"),
  parseJsonValue "[]" (JsonList []),
  parseJsonValue "[1]" (JsonList [JsonInt 1]),
  parseJsonValue "[1, 2]" (JsonList [JsonInt 1, JsonInt 2]),
  parseJsonValue "[1, 2, \"asd\"]" (JsonList [JsonInt 1, JsonInt 2, JsonString "asd"]),
  parseJsonValue "{}" (JsonMap []),
  parseJsonValue "{\"a\":2}" (JsonMap [(JsonString "a", JsonInt 2)]),
  parseJsonValue "{\"a\":2, \"b\":3}" (JsonMap [(JsonString "a", JsonInt 2), (JsonString "b", JsonInt 3)])]

parseJsonValue :: String -> JsonValue -> TestTree
parseJsonValue jsonString expectedJsonValue = testCase ("Parse json string '" ++ jsonString ++"' and get " ++ (show expectedJsonValue))
  (assertEqual "should parse json string" (getValue $ apply jvalue jsonString) expectedJsonValue)
