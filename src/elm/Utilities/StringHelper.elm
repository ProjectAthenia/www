module Utilities.StringHelper exposing (..)

import List.Extra as ListExtra


punctuationChars : List Char
punctuationChars =
    [' ', '\n', '\r', '\t' -- Basic Spacing characters
    , '.', ',', '(', ')', '\'', '"', '`', ';', ':', '!', '?', '·', '…', '…' -- Common punctuation
    , '~', '\\', '/', '+', '=', '-', '_', '%', '*' -- Mostly math stuff
    , '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
    , '￥', '$', '¢' -- Currency stuff
    , '。', '，', '；', '：', '、', '《', '》', '【', '】', '）', '（', '「', '」', '﹁', '﹂', '？', '！' -- Chinese stuff
    ]


isPunctuation : Char -> Bool
isPunctuation character =
    List.member character punctuationChars


transformToIndexedList : String -> List (Int, Char)
transformToIndexedList content =
    ListExtra.zip (List.range 0 (String.length content)) (String.toList content)


isCharTupleEqual: (Char, Char) -> Bool
isCharTupleEqual (charA, charB) =
    charA == charB


areCharsNotEqualAtPosition: Int -> (Char, Char) -> Maybe Int
areCharsNotEqualAtPosition position charTuple =
    if isCharTupleEqual charTuple then
        Nothing
    else
        Just position


findNotMatchingPositions: List (Char, Char) -> List (Int)
findNotMatchingPositions charTuples =
    List.filterMap (\maybePosition -> maybePosition)
        <| List.indexedMap areCharsNotEqualAtPosition charTuples


findFirstNoneMatchingCharTuplePosition: List (Char, Char) -> Maybe Int
findFirstNoneMatchingCharTuplePosition charTuples =
    List.head
        <| findNotMatchingPositions charTuples


convertStringsIntoCharTuples: String -> String -> List (Char, Char)
convertStringsIntoCharTuples stringA stringB =
    List.map2 Tuple.pair (String.toList stringA) (String.toList stringB)


findFirstNoneMatchingStringPosition: String -> String -> Maybe Int
findFirstNoneMatchingStringPosition stringA stringB =
    findFirstNoneMatchingCharTuplePosition
        <| convertStringsIntoCharTuples (stringA ++ "\n") (stringB ++ "\n")


findLastNoneMatchingStringPosition: String -> String -> Maybe Int
findLastNoneMatchingStringPosition stringA stringB =
    findFirstNoneMatchingCharTuplePosition
        <| convertStringsIntoCharTuples ((String.reverse stringA) ++ "\n") ((String.reverse stringB) ++ "\n")

