module UnitTests.Utilities.AuthManagerTest exposing (..)

import Athenia.Utilities.AuthManager as AuthManager
import Expect
import Test exposing (..)
import Time


testNeedsRefresh : Test
testNeedsRefresh =
    describe "Makes sure that the needs refresh function works properly"
        [ test "should return false at 5 minutes" <|
            \() ->
                Expect.false ""
                    <| AuthManager.needsRefresh (Time.millisToPosix (5 * 60 * 1000)) 0
        , test "should return true at 55 minutes" <|
            \() ->
                Expect.true ""
                    <| AuthManager.needsRefresh (Time.millisToPosix (55 * 60 * 1000)) 0
        ]