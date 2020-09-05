module CalendarDates exposing (days, weeks, Day, Week)

{-| Generate date ranges.

@docs days, weeks, Day, Week

-}

import Calendar
import Date exposing (Date, Interval)
import Derberos.Date.Utils exposing (getNextMonth, getPrevWeekday, numberOfDaysInMonth)
import List.Extra
import Time exposing (Month, Weekday)


{-| Day
-}
type alias Day =
    { date : Calendar.Date
    , month : Order
    }


{-| Week
-}
type alias Week =
    { mon : Day
    , tue : Day
    , wed : Day
    , thu : Day
    , fri : Day
    , sat : Day
    , sun : Day
    }


toInterval : Weekday -> Interval
toInterval wd =
    case wd of
        Time.Mon ->
            Date.Monday

        Time.Tue ->
            Date.Tuesday

        Time.Wed ->
            Date.Wednesday

        Time.Thu ->
            Date.Thursday

        Time.Fri ->
            Date.Friday

        Time.Sat ->
            Date.Saturday

        Time.Sun ->
            Date.Sunday


emptyWeek : Week
emptyWeek =
    let
        d =
            { date =
                0
                    |> Time.millisToPosix
                    |> Calendar.fromPosix
            , month = EQ
            }
    in
    Week d d d d d d d


range : Date -> Date -> List Date
range a b =
    Date.range Date.Day
        1
        (Date.min a b)
        (Date.add Date.Days 1 (Date.max a b))


{-| Individual days.
-}
days : Weekday -> Month -> Int -> List Day
days wd month year =
    let
        start =
            Date.fromCalendarDate year month 1

        gap =
            Date.diff Date.Days start (Date.floor (toInterval wd) start)

        padLeft =
            (if Date.weekday start == wd then
                []

             else
                List.range gap -1
            )
                |> List.map
                    (\i ->
                        Date.add Date.Days i start
                    )

        until =
            start
                |> Date.add Date.Days (numberOfDaysInMonth year month - 1)

        gap2 =
            Date.diff Date.Days
                until
                (Date.ceiling (toInterval (getPrevWeekday wd)) until)

        padRight =
            (if Date.weekday (Date.add Date.Days 1 until) == wd then
                []

             else if Date.weekday until == wd then
                List.range 1 6

             else
                List.range 1 gap2
            )
                |> List.map
                    (\i ->
                        Date.add Date.Days i until
                    )
    in
    padLeft
        ++ range start until
        ++ padRight
        |> List.map
            (\date ->
                { date =
                    Calendar.RawDate
                        (Date.year date)
                        (Date.month date)
                        (Date.day date)
                        |> Calendar.fromRawParts
                        |> Maybe.withDefault default
                , month =
                    if Date.month date == month then
                        EQ

                    else if Date.month date == getNextMonth month then
                        GT

                    else
                        LT
                }
            )


default : Calendar.Date
default =
    0 |> Time.millisToPosix |> Calendar.fromPosix


{-| Days split into weeks.
-}
weeks : Weekday -> Month -> Int -> List Week
weeks wd month year =
    days wd month year
        |> List.Extra.groupsOf 7
        |> List.map
            (\xs ->
                xs
                    |> List.foldl
                        (\day ->
                            case Calendar.getWeekday day.date of
                                Time.Mon ->
                                    \week ->
                                        { week | mon = day }

                                Time.Tue ->
                                    \week ->
                                        { week | tue = day }

                                Time.Wed ->
                                    \week ->
                                        { week | wed = day }

                                Time.Thu ->
                                    \week ->
                                        { week | thu = day }

                                Time.Fri ->
                                    \week ->
                                        { week | fri = day }

                                Time.Sat ->
                                    \week ->
                                        { week | sat = day }

                                Time.Sun ->
                                    \week ->
                                        { week | sun = day }
                        )
                        emptyWeek
            )
