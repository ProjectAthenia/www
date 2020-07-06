module UnitTests.Models.MembershipPlan.MembershipPlanTest exposing (..)

import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)


testModelDecoder : Test
testModelDecoder =
    test "Test decode" <|
        \() ->
            Expect.equal (Ok { id = Just 342
                            , name = "Steve"
                            , duration = "yearly"
                            , current_cost = 12.12
                            , current_rate_id = 345
                            })
                <| JsonDecode.decodeString MembershipPlan.modelDecoder "{\"id\":342,\"name\":\"Steve\",\"duration\":\"yearly\",\"current_cost\":\"12.12\",\"current_rate_id\":345}"
