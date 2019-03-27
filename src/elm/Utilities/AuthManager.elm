module Utilities.AuthManager exposing (..)

import Time

needsRefresh : Time.Posix -> Int -> Bool
needsRefresh currentTime lastRefreshTime =
    Time.posixToMillis currentTime - (55 * 60 * 1000) >= lastRefreshTime