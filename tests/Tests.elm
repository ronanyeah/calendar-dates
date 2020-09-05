module Tests exposing (all)

import CalendarDates
import Expect
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Calendar"
        [ test "days" <|
            \() ->
                CalendarDates.days Time.Mon Time.Jan 2000
                    |> List.length
                    |> Expect.equal 42
        , test "weeks" <|
            \() ->
                CalendarDates.weeks Time.Mon Time.Dec 2019
                    |> List.length
                    |> Expect.equal 6
        ]
