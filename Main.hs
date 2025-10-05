import Data.Either (isLeft) -- For checking if the result is Left
import Data.Map qualified as Map
import Parser


-- Your parse function signature
-- parse :: String -> Either String Json

-- Helper to make expected results cleaner (same as before)
s :: String -> Json
s = Str

n :: Double -> Json
n = Num

b :: Bool -> Json
b True = JTrue
b False = JFalse

obj :: [(String, Json)] -> Json
obj = Obj . Map.fromList

arr :: [Json] -> Json
arr = Arr

isNull :: Json
isNull = Null

-- Define what an expected outcome looks like
-- For errors, we just care that it's a Left.
-- For success, we care about the exact Json value.
data ExpectedResult
  = ExpectSuccess Json
  | ExpectFailure
  deriving (Show, Eq)

testCases :: [(String, ExpectedResult)]
testCases =
  [ -- Basic Valid Cases
    ("null", ExpectSuccess Null),
    ("true", ExpectSuccess JTrue),
    ("false", ExpectSuccess JFalse),
    ("\"hello world\"", ExpectSuccess (Str "hello world")),
    ("123", ExpectSuccess (Num 123.0)),
    ("123.456", ExpectSuccess (Num 123.456)),
    ("-78.9", ExpectSuccess (Num (-78.9))),
    ("1.2e3", ExpectSuccess (Num 1200.0)),
    ("1.2E-2", ExpectSuccess (Num 0.012)),
    ("0", ExpectSuccess (Num 0.0)),
    ("-0.5", ExpectSuccess (Num (-0.5))),
    -- Empty Structures
    ("{}", ExpectSuccess (obj [])),
    ("[]", ExpectSuccess (arr [])),
    -- Simple Object
    ("{\"key\": \"value\"}", ExpectSuccess (obj [("key", Str "value")])),
    ("{\"number\": 101}", ExpectSuccess (obj [("number", Num 101.0)])),
    ("{\"bool\": true}", ExpectSuccess (obj [("bool", JTrue)])),
    ("{\"null_val\": null}", ExpectSuccess (obj [("null_val", Null)])),
    -- Simple Array
    ("[1, 2, 3]", ExpectSuccess (arr [Num 1.0, Num 2.0, Num 3.0])),
    ("[\"a\", \"b\", \"c\"]", ExpectSuccess (arr [Str "a", Str "b", Str "c"])),
    ("[true, false, null]", ExpectSuccess (arr [JTrue, JFalse, Null])),
    -- Mixed Array
    ("[\"hello\", 1.23, true, null, false]", ExpectSuccess (arr [Str "hello", Num 1.23, JTrue, Null, JFalse])),
    -- Nested Object
    ( "{\"name\": \"test\", \"data\": {\"value\": 10, \"active\": false}}",
      ExpectSuccess
        ( obj
            [ ("name", Str "test"),
              ("data", obj [("value", Num 10.0), ("active", JFalse)])
            ]
        )
    ),
    -- Nested Array
    ("[[1, 2], [3, 4]]", ExpectSuccess (arr [arr [Num 1.0, Num 2.0], arr [Num 3.0, Num 4.0]])),
    ("[\"a\", [1, true]]", ExpectSuccess (arr [Str "a", arr [Num 1.0, JTrue]])),
    -- Object with Array Value
    ("{\"items\": [1, \"two\", null]}", ExpectSuccess (obj [("items", arr [Num 1.0, Str "two", Null])])),
    -- Array with Object Values
    ( "[{\"id\": 1, \"name\": \"foo\"}, {\"id\": 2, \"name\": \"bar\"}]",
      ExpectSuccess
        ( arr
            [ obj [("id", Num 1.0), ("name", Str "foo")],
              obj [("id", Num 2.0), ("name", Str "bar")]
            ]
        )
    ),
    -- Strings with Escapes
    ("\"\\\"hello\\\\world\\/\\b\\f\\n\\r\\t\"", ExpectSuccess (Str "\"hello\\world/\b\f\n\r\t")),
    ("\"\\u004A\\u0061\\u0073\\u006F\\u006E\"", ExpectSuccess (Str "Jason")),
    -- Whitespace Handling
    ("  {  \"key\"  :  \"value\"  }  ", ExpectSuccess (obj [("key", Str "value")])),
    ("  [  1  ,  2  ,  3  ]  ", ExpectSuccess (arr [Num 1.0, Num 2.0, Num 3.0])),
    ("{\n  \"name\": \"whitespace test\",\n  \"value\": true\n}", ExpectSuccess (obj [("name", Str "whitespace test"), ("value", JTrue)])),
    ("[\n1,\n2\n]", ExpectSuccess (arr [Num 1.0, Num 2.0])),
    -- Edge case numbers
    ("1e+0", ExpectSuccess (Num 1.0)),
    ("1e-0", ExpectSuccess (Num 1.0)),
    ("0.0", ExpectSuccess (Num 0.0)),
    ("0e0", ExpectSuccess (Num 0.0)),
    -- Valid complex nested structure
    ( "{\"a\":[1,{\"b\":true,\"c\":[null,false,\"foo\"]}],\"d\":{\"e\":-10.5}}",
      ExpectSuccess
        ( obj
            [ ("a", arr [Num 1.0, obj [("b", JTrue), ("c", arr [Null, JFalse, Str "foo"])]]),
              ("d", obj [("e", Num (-10.5))])
            ]
        )
    ),
    -- Invalid JSON Cases (expecting any Left)
    ("", ExpectFailure),
    ("   ", ExpectFailure),
    ("{", ExpectFailure),
    ("}", ExpectFailure),
    ("[", ExpectFailure),
    ("]", ExpectFailure),
    ("{\"key\": \"value\"", ExpectFailure),
    ("[\"item1\", \"item2\"", ExpectFailure),
    ("{\"key\": value_no_quotes}", ExpectFailure),
    ("{key_no_quotes: \"value\"}", ExpectFailure),
    ("{\"key\": \"value\",}", ExpectFailure), -- Trailing comma in object
    ("[\"item1\", \"item2\",]", ExpectFailure), -- Trailing comma in array
    ("{\"key1\": \"v1\" \"key2\": \"v2\"}", ExpectFailure),
    ("[\"v1\" \"v2\"]", ExpectFailure),
    ("abc", ExpectFailure),
    ("True", ExpectFailure),
    ("FALSE", ExpectFailure),
    ("NULL", ExpectFailure),
    ("ture", ExpectFailure),
    ("fals", ExpectFailure),
    ("nul", ExpectFailure),
    ("\"unterminated string", ExpectFailure),
    ("\"string with unescaped \n newline\"", ExpectFailure),
    ("\"string with invalid escape \\x \"", ExpectFailure),
    ("1.2.3", ExpectFailure),
    ("1.e", ExpectFailure),
    ("1.E+", ExpectFailure),
    ("-.E1", ExpectFailure),
    ("1a", ExpectFailure),
    ("{ \"key\": }", ExpectFailure),
    ("{ : \"value\" }", ExpectFailure),
    ("[1, , 2]", ExpectFailure),
    ("[,1,2]", ExpectFailure),
    ("{\"key\": \"value\"}trailing_text", ExpectFailure),
    ("[1,2,3]trailing_text", ExpectFailure),
    ("{\"key\": 01}", ExpectFailure),
    ("{\"key\": .1}", ExpectFailure),
    ("{\"key\": +.1}", ExpectFailure)
  ]

-- Example of how you might run these tests with the new ExpectedResult
-- (Assuming 'parse' is your parsing function: parse :: String -> Either String Json)

-- parse :: String -> Either String Json -- Your actual parser function
-- parse s = if s == "null" then Right Null else Left "dummy error" -- Dummy for testing the test runner

main :: IO ()
main = do
  putStrLn "Running JSON Parser Tests (Checking Left for errors)..."
  mapM_ runTest testCases
  where
    runTest :: (String, ExpectedResult) -> IO ()
    runTest (input, expectedOutcome) = do
      let actualResult = parse input -- Call your parser here
      let pass = case (expectedOutcome, actualResult) of
            (ExpectSuccess expectedJson, Right actualJson) -> expectedJson == actualJson
            (ExpectFailure, Left _) -> True
            _ -> False -- Mismatch (e.g., expected success, got failure, or vice-versa, or wrong success value)
      if pass
        then putStrLn $ "PASS: " ++ ellipses input
        else
          putStrLn $
            "FAIL: "
              ++ ellipses input
              ++ "\n  Expected: "
              ++ showExpectedOutcome expectedOutcome actualResult
              ++ "\n  Actual:   "
              ++ showActualResult actualResult
    ellipses s
      | length s > 40 = take 37 s ++ "..."
      | otherwise = s

    showExpectedOutcome :: ExpectedResult -> Either String Json -> String
    showExpectedOutcome (ExpectSuccess json) _ = "Right " ++ show json
    showExpectedOutcome ExpectFailure (Right actualJson) = "Left _ (but was Right " ++ show actualJson ++ ")"
    showExpectedOutcome ExpectFailure (Left _) = "Left _"

    showActualResult :: Either String Json -> String
    showActualResult (Right json) = "Right " ++ show json
    showActualResult (Left errMsg) = "Left \"" ++ errMsg ++ "\"" -- Still show the error for debugging if it fails
