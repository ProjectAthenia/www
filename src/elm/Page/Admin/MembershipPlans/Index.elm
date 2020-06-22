module Page.Admin.MembershipPlans.Index exposing (..)
import Html exposing (..)
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Components.CRUD.ModelList as ModelList
import Utilities.Expands as Expands
import Utilities.ModelHelpers as ModelHelpers
import Utilities.SearchField as SearchField


-- All Index Stuff starts here

indexConfiguration : List Expands.Expand -> String -> (ModelList.Configuration MembershipPlan.Model)
indexConfiguration expands apiUrl =
    let
        configuration =
            ModelList.configure "concepts" (MembershipPlan.routeGroup apiUrl) "Concept" expands [] MembershipPlan.pageDecoder processDataResponse
                <|  [ ModelList.column "Name" rowName "name" SearchField.Text
                    , ModelList.column "Duration" rowDuration "duration"
                        <| SearchField.Select
                            [ ("", "All")
                            , ("yearly", "Yearly")
                            , ("monthly", "Monthly")
                            ]
                    , ModelList.column "Current Cost" rowDuration "duration" SearchField.None
                    ]
    in
    ModelList.enableDelete configuration


processDataResponse : List MembershipPlan.Model -> List (Int, MembershipPlan.Model)
processDataResponse models =
    ModelHelpers.toKeyValuePairs models


rowName : MembershipPlan.Model -> Html (ModelList.Msg MembershipPlan.Model)
rowName membershipPlan =
    text membershipPlan.name


rowDuration : MembershipPlan.Model -> Html (ModelList.Msg MembershipPlan.Model)
rowDuration membershipPlan =
    text membershipPlan.duration


rowCost : MembershipPlan.Model -> Html (ModelList.Msg MembershipPlan.Model)
rowCost membershipPlan =
    text
        <| "$" ++ String.fromFloat membershipPlan.current_cost