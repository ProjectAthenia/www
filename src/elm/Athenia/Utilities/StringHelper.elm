module Athenia.Utilities.StringHelper exposing (..)


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

