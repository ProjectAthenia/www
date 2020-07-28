module Utilities.DateHelpers exposing (..)

import DateFormat
import Time exposing (..)


format: Zone -> Posix -> String
format =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text ", "
        , DateFormat.yearNumber
        ]


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
