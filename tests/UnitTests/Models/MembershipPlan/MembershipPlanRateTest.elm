module UnitTests.Models.MembershipPlan.MembershipPlanRateTest exposing (..)

import Models.MembershipPlan.MembershipPlanRate as MembershipPlanRate
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)

testModelDecoder : Test
testModelDecoder =
    test "Test decode" <|
        \() ->
            Expect.equal (Ok { id = 342
                            , cost = 12.12
                            })
                <| JsonDecode.decodeString MembershipPlanRate.modelDecoder "{\"id\":342,\"cost\":12.12}"