module Page.Admin.MembershipPlan exposing (..)

import Components.CRUD.SharedConfiguration as SharedConfiguration
import Components.CRUD.ModelForm as ModelForm
import Components.CRUD.ModelList as ModelList
import Components.CRUD.RootController as RootController
import Html
import Models.MembershipPlan.MembershipPlan as MembershipPlan
import Utilities.SearchField exposing (SearchFieldType(..))


sharedConfiguration: String -> SharedConfiguration.Configuration MembershipPlan.Model
sharedConfiguration apiUrl =
    SharedConfiguration.configure "Membership Plans" "membership-plans" (MembershipPlan.routeGroup apiUrl) MembershipPlan.modelDecoder []


nameColumn: ModelList.Column MembershipPlan.Model
nameColumn =
     ModelList.column
        "Name"
        (\membershipPlan -> Html.text membershipPlan.name)
        "name"
        Text


durationColumn: ModelList.Column MembershipPlan.Model
durationColumn =
     ModelList.column
        "Duration"
        (\membershipPlan -> Html.text membershipPlan.duration)
        "duration"
        (Select [("lifetime", "Lifetime"), ("monthly", "Monthly"), ("yearly", "Yearly")])


currentCostColumn: ModelList.Column MembershipPlan.Model
currentCostColumn =
     ModelList.column
        "Current Cost"
        (\membershipPlan -> Html.text (String.fromFloat membershipPlan.current_cost))
        ""
        None


indexColumns: List (ModelList.Column MembershipPlan.Model)
indexColumns =
    [ nameColumn
    , durationColumn
    , currentCostColumn
    ]

indexConfiguration: ModelList.Configuration MembershipPlan.Model
indexConfiguration =
    ModelList.configure [] indexColumns