module UnitTests.MainTest exposing (..)

import Expect
import Main
import Models.Role as Role
import Page.Admin.Root as Admin
import Route
import Test exposing (..)
import UnitTests.Models.User.UserTest as UserTest


testGetNavItems: Test
testGetNavItems =
    describe "Tests to make sure that the get nav items returns the right links based on auth state"
        [ test "Should Return the logged out nav without a token" <|
            \() ->
                Expect.equal (Main.getNavItems Nothing Nothing)
                    [ ("Sign In", Route.href  Route.Login)
                    , ("Sign Up", Route.href  Route.SignUp)
                    ]
        , test "Should Return the logged in nav with a token" <|
            \() ->
                Expect.equal (Main.getNavItems (Just ()) Nothing)
                    [ ("Settings", Route.href  Route.Settings)
                    , ("Admin", Route.href (Route.Admin Admin.Dashboard))
                    , ("Log Out", Route.href  Route.Logout)
                    ]
        , test "Should Return the logged in nav with a token, and with a user that cannot view articles" <|
            \() ->
                let
                    user = UserTest.mockUser "" "" "" []
                in
                Expect.equal (Main.getNavItems (Just ()) (Just user))
                    [ ("Settings", Route.href  Route.Settings)
                    , ("Admin", Route.href (Route.Admin Admin.Dashboard))
                    , ("Log Out", Route.href  Route.Logout)
                    ]
        , test "Should Return the logged in nav with a token, and with a user that can view articles" <|
            \() ->
                let
                    user = UserTest.mockUser "" "" "" [{id = Role.articleViewer, name = ""}]
                in
                Expect.equal (Main.getNavItems (Just ()) (Just user))
                    [ ("Browse Articles", Route.href Route.Articles)
                    , ("Settings", Route.href  Route.Settings)
                    , ("Admin", Route.href (Route.Admin Admin.Dashboard))
                    , ("Log Out", Route.href  Route.Logout)
                    ]
        ]
