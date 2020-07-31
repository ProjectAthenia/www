module UnitTests.Models.MembershipPlan.MembershipPlanRateTest exposing (..)

import Models.MembershipPlan.MembershipPlanRate as MembershipPlanRate
import Expect
import Json.Decode as JsonDecode
import Test exposing (..)
import Time exposing (millisToPosix)


testModelDecoder : Test
testModelDecoder =
    test "Test decode" <|
        \() ->
            Expect.equal (Ok { id = 342
                            , cost = 12.12
                            , created_at = Just <| millisToPosix 2000
                            , membership_plan = Just
                                { id = Just 342
                                , name = "Steve"
                                , duration = "yearly"
                                , current_cost = 12.12
                                , current_rate_id = 345
                                }
                            })
                <| JsonDecode.decodeString MembershipPlanRate.modelDecoder "{\"id\":342,\"cost\":\"12.12\",\"created_at\":\"1970-01-01T00:00:02+00:00\",\"membership_plan\":{\"id\":342,\"name\":\"Steve\",\"duration\":\"yearly\",\"current_cost\":\"12.12\",\"current_rate_id\":345}}"