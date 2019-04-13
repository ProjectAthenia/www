module Utilities.DateHelpers exposing (..)

import Time exposing (..)


getMonthName : Month -> String
getMonthName month =
    case month of
        Jan ->
            "January"
        Feb ->
            "February"
        Mar ->
            "March"
        Apr ->
            "April"
        May ->
            "May"
        Jun ->
            "June"
        Jul ->
            "July"
        Aug ->
            "August"
        Sep ->
            "September"
        Oct ->
            "October"
        Nov ->
            "November"
        Dec ->
            "December"
