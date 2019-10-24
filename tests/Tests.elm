module Tests exposing (all)

import Calendar
import Expect
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Calendar"
        [ test "days" <|
            \() ->
                Calendar.days Time.Mon Time.Jan 2000
                    |> List.length
                    |> Expect.equal 42
        , test "weeks" <|
            \() ->
                Calendar.weeks Time.Mon Time.Dec 2019
                    |> List.length
                    |> Expect.equal 6
        ]
