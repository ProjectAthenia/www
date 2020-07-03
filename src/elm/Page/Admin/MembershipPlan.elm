module Page.Admin.MembershipPlan exposing (..)

import Components.CRUD.SharedConfiguration as SharedConfiguration
import Components.CRUD.ModelForm as ModelForm
import Components.CRUD.ModelList as ModelList
import Components.CRUD.RootController as RootController
import Models.MembershipPlan.MembershipPlan as MembershipPlan


sharedConfiguration: String -> SharedConfiguration.Configuration MembershipPlan.Model
sharedConfiguration apiUrl =
    SharedConfiguration.configure "Membership Plans" "membership-plans" (MembershipPlan.routeGroup apiUrl) MembershipPlan.modelDecoder []