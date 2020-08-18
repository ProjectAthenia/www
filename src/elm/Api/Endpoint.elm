module Api.Endpoint exposing
    ( Endpoint, request, url
    , baseArticle, viewArticle, viewArticles
    , viewArticleIterations
    , login, signUp, refresh
    , forgotPassword
    , roles
    , membershipPlans, membershipPlanRates
    , user, userActivity, me
    , userPaymentMethods, entityPayments, entityPayment, entitySubscriptions, entitySubscription
    )

import Http
import Url.Builder as Builder exposing (QueryParameter)
import Utilities.Expands as Expands


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request :
    { method : String
    , headers : List Http.Header
    , url : Endpoint
    , body : Http.Body
    , expect : Http.Expect msg
    , timeout : Maybe Float
    }
    -> Cmd msg
request config =
    Http.request
        { method = config.method
        , headers = config.headers
        , url = unwrap config.url
        , body = config.body
        , expect = config.expect
        , timeout = config.timeout
        , tracker = Nothing
        }

-- TYPES


{-| Get a URL to the API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap : Endpoint -> String
unwrap (Endpoint str) =
    str


url : String -> List String -> List QueryParameter -> Endpoint
url apiUrl paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Builder.crossOrigin apiUrl
        (paths)
        queryParams
        |> Endpoint



-- System wide endpoints


baseArticle : String -> Endpoint
baseArticle apiUrl =
    url apiUrl [ "articles" ] []

viewArticle : String -> Int -> Endpoint
viewArticle apiUrl articleId =
    url apiUrl [ "articles", String.fromInt articleId ]
        [ (Builder.string "expand[createdBy]" "*")
        ]


viewArticles : String -> Int -> Endpoint
viewArticles apiUrl page =
    url apiUrl [ "articles" ]
        <| if page /= 1 then
            [ (Builder.string "page" (String.fromInt page))
            , (Builder.string "expand[createdBy]" "*")
            ]
        else
            [ (Builder.string "expand[createdBy]" "*") ]

viewArticleIterations : String -> Int -> Int -> Endpoint
viewArticleIterations apiUrl articleId page =
    url apiUrl [ "articles", String.fromInt articleId, "iterations" ]
        <| if page /= 1 then
            [ (Builder.string "page" (String.fromInt page))
            , (Builder.string "expand[createdBy]" "*")
            , (Builder.string "limit" "100")
            ]
        else
            [ (Builder.string "expand[createdBy]" "*")
            , (Builder.string "limit" "100")
            ]


me : String -> Endpoint
me apiUrl =
    url apiUrl [ "users", "me" ]
        [ (Builder.string "expand[roles]" "*")
        , (Builder.string "expand[paymentMethods]" "*")
        , (Builder.string "expand[subscriptions]" "*")
        , (Builder.string "expand[subscriptions.paymentMethod]" "*")
        , (Builder.string "expand[subscriptions.membershipPlanRate]" "*")
        , (Builder.string "expand[subscriptions.membershipPlanRate.membershipPlan]" "*")
        ]


refresh : String -> Endpoint
refresh apiUrl =
    url apiUrl [ "auth", "refresh" ] []


roles : String -> Endpoint
roles apiUrl =
    url apiUrl [ "roles" ] []


login : String -> Endpoint
login apiUrl =
    url apiUrl [ "auth", "login" ] []


logout : String -> Endpoint
logout apiUrl =
    url apiUrl [ "auth", "logout" ] []


signUp : String -> Endpoint
signUp apiUrl =
    url apiUrl [ "auth", "sign-up" ] []


forgotPassword : String -> Endpoint
forgotPassword apiUrl =
    url apiUrl [ "forgot-password" ] []


membershipPlans : String -> Endpoint
membershipPlans apiUrl =
    url apiUrl [ "membership-plans" ] []


membershipPlanRates: String -> Int -> Int -> Endpoint
membershipPlanRates apiUrl id page =
    url apiUrl [ "membership-plans", String.fromInt id, "rates" ]
        [ Builder.int "page" page
        ]


user : String -> Int -> Endpoint
user apiUrl userId =
    url apiUrl [ "users", String.fromInt userId ] []


userPaymentMethods : String -> Int -> Endpoint
userPaymentMethods apiUrl userId =
    url apiUrl [ "users", String.fromInt userId, "payment-methods" ] []


entityPayments : String -> String -> Int -> Int -> Endpoint
entityPayments apiUrl entityType entityId page =
    url apiUrl [ entityType, String.fromInt entityId, "payments" ]
        <| [ Builder.int "page" page ]


entityPayment : String -> String -> Int -> Int -> Endpoint
entityPayment apiUrl entityType userId paymentId =
    url apiUrl [ entityType, String.fromInt userId, "payments", String.fromInt paymentId ] []


entitySubscriptions : String -> String -> Int -> Int -> Endpoint
entitySubscriptions apiUrl entityType entityId page =
    url apiUrl [ entityType, String.fromInt entityId, "subscriptions" ]
        <| List.concat
            [ [ Builder.int "page" page ]
            , Expands.toQueryParameters
                [ Expands.expand "membershipPlanRate"
                , Expands.expand "membershipPlanRate.membershipPlan"
                ]
            ]


entitySubscription : String -> String -> Int -> Int -> Endpoint
entitySubscription apiUrl entityType userId subscriptionId =
    url apiUrl [ entityType, String.fromInt userId, "subscriptions", String.fromInt subscriptionId ] []


userActivity : String -> Int -> Endpoint
userActivity apiUrl userId =
    url apiUrl [ "users", String.fromInt userId ]
        [ (Builder.string "expand[createdArticles]" "*")
        , (Builder.string "expand[createdIterations]" "*")
        ]
