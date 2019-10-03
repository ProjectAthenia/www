module UnitTests.Utilities.StringHelperTest exposing (..)

import Utilities.StringHelper as StringHelper
import Expect
import Test exposing (..)


testIsPunctuation : Test
testIsPunctuation =
    describe "Check to make sure that various characters pass the is puncuation test"
        [ test "Makes sure a chinese character passes the test as false" <|
            \() ->
                Expect.false "" <|
                    StringHelper.isPunctuation '冰'
        , test "Makes sure a chinese punctuation character passes the test as true" <|
            \() ->
                Expect.true "" <|
                    StringHelper.isPunctuation '。'
        ]


testTransformToIndexedList : Test
testTransformToIndexedList =
    test "Makes sure we can tranform a string properly" <|
        \() ->
            Expect.equal [(0, '我'), (1, '是'), (2, '肉'), (3, '。')] <|
                StringHelper.transformToIndexedList "我是肉。"


testIsCharTupleEqual: Test
testIsCharTupleEqual =
    describe "Makes sure that the isCharTupleEqual comparison works"
        [ test "should match two characters" <|
            \() ->
                Expect.true ""
                    <| StringHelper.isCharTupleEqual ('a', 'a')
        , test "should fail with two mismatched characters" <|
            \() ->
                Expect.false ""
                    <| StringHelper.isCharTupleEqual ('a', 'b')
        ]


testAreCharsEqualAtPosition: Test
testAreCharsEqualAtPosition =
    describe "Makes sure that the areCharsEqualAtPosition comparison works"
        [ test "should match two characters, and return the position" <|
            \() ->
                Expect.equal Nothing
                    <| StringHelper.areCharsNotEqualAtPosition 43 ('a', 'a')
        , test "should fail with two mismatched characters" <|
            \() ->
                Expect.equal (Just 54)
                    <| StringHelper.areCharsNotEqualAtPosition 54 ('a', 'b')
        ]


testFindNotMatchingPositions: Test
testFindNotMatchingPositions =
    test "Makes sure that the values are mapped properly" <|
        \() ->
            Expect.equal [2]
                <| StringHelper.findNotMatchingPositions [('a', 'a'), ('b', 'b'), ('c', 'd'), ('e', 'e')]


testFindFirstNoneMatchingCharTuplePosition: Test
testFindFirstNoneMatchingCharTuplePosition =
    describe "Makes sure that we can find the first position under certain scenarios"
        [ test "should return the position when there is a match" <|
            \() ->
                Expect.equal (Just 2)
                    <| StringHelper.findFirstNoneMatchingCharTuplePosition [('a', 'a'), ('b', 'b'), ('c', 'd'), ('e', 'e')]
        , test "should return nothing when there is no match" <|
            \() ->
                Expect.equal Nothing
                    <| StringHelper.findFirstNoneMatchingCharTuplePosition [('a', 'a'), ('b', 'b'), ('c', 'c'), ('e', 'e')]
        ]


testConvertStringsIntoCharTuples: Test
testConvertStringsIntoCharTuples =
    test "Makes sure that the conversion is happening properly" <|
        \() ->
            Expect.equal [('a', 'a'), ('b', 'b'), ('c', 'd')]
                <| StringHelper.convertStringsIntoCharTuples "abc" "abd"


testFindFirstNoneMatchingStringPosition: Test
testFindFirstNoneMatchingStringPosition =
    describe "Makes sure that we can find the first none matching position under certain scenarios"
        [ test "should return the position when there is a none matching string" <|
            \() ->
                Expect.equal (Just 2)
                    <| StringHelper.findFirstNoneMatchingStringPosition "abce" "abdef"
        , test "should return nothing when the strings match" <|
            \() ->
                Expect.equal Nothing
                    <| StringHelper.findFirstNoneMatchingStringPosition "abcde" "abcde"
        , test "should return the last digit when the difference is at the end" <|
            \() ->
                Expect.equal (Just 5)
                    <| StringHelper.findFirstNoneMatchingStringPosition "Hello" "Hello Hi"
        ]


testFindLastNoneMatchingStringPosition: Test
testFindLastNoneMatchingStringPosition =
    describe "Makes sure that we can find the last none matching position under certain scenarios"
        [ test "should return the position when there is a none matching string" <|
            \() ->
                Expect.equal (Just 2)
                    <| StringHelper.findLastNoneMatchingStringPosition "bcef" "abdef"
        , test "should return nothing when the strings match" <|
            \() ->
                Expect.equal Nothing
                    <| StringHelper.findLastNoneMatchingStringPosition "abcde" "abcde"
        ]
