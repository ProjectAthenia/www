module Components.MembershipPlan.RateHistory exposing (..)

import Models.MembershipPlan.MembershipPlanRate as MembershipPlanRate


type alias Model =
    { membershipPlanId: Int
    , loadedRates: List MembershipPlanRate.Model
    }
