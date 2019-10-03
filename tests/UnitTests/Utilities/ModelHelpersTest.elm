module UnitTests.Utilities.ModelHelpersTest exposing(..)

import Expect
import Utilities.ModelHelpers as ModelHelpers
import Test exposing (..)


testPluckIds : Test
testPluckIds =
    test "Makes sure that we can turn a list of models into a list of ids" <|
        \() ->
            Expect.equal [54, 2435, 65] <|
                ModelHelpers.pluckIds [{ id = Just 54},{ id = Just 2435},{ id = Just 65}]


testToKeyValuePairs : Test
testToKeyValuePairs =
    test "Makes sure that we can convert a list of modesl into key value pairs with the id as the key" <|
        \() ->
            Expect.equal [(54, { id = Just 54}), (2435, { id = Just 2435}), (65, { id = Just 65})] <|
                ModelHelpers.toKeyValuePairs [{ id = Just 54},{ id = Just 2435},{ id = Just 65}]


testPullFromKeyValueById : Test
testPullFromKeyValueById =
    describe "Makes sure that we can pull a model by it's id properly"
        [ test "Makes sure that a model is returned when it is found" <|
            \() ->
                Expect.equal (Just { id = Just 54}) <|
                    ModelHelpers.pullFromKeyValueById 54 [(54, { id = Just 54}), (2435, { id = Just 2435}), (65, { id = Just 65})]
        , test "Makes sure that nothing is returned when it does not exist" <|
            \() ->
                Expect.equal Nothing <|
                    ModelHelpers.pullFromKeyValueById 51 [(54, { id = Just 54}), (2435, { id = Just 2435}), (65, { id = Just 65})]
        ]
