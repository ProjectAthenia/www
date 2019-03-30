module UnitTests.MainTest exposing (..)

import Expect
import Main
import Route
import Test exposing (..)


testGetNavItems: Test
testGetNavItems =
    describe "Tests to make sure that the get nav items returns the right links based on auth state"
        [ test "Should Return the logged out nav without a token" <|
            \() ->
                Expect.equal (Main.getNavItems Nothing)
                    [ ("Sign In", Route.href  Route.Login)
                    , ("Sign Up", Route.href  Route.SignUp)
                    ]
        , test "Should Return the logged in nav with a token" <|
            \() ->
                Expect.equal (Main.getNavItems (Just ()))
                    [ ("Browse Articles", Route.href Route.Articles)
                    , ("Settings", Route.href  Route.Settings)
                    , ("Log Out", Route.href  Route.Logout)
                    ]
        ]
