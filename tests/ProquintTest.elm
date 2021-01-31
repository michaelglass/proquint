module ProquintTest exposing (suite)

import Bitwise
import Expect
import Proquint exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Proquint"
        [ describe
            "golden tests"
            ([ ( Ip 127 0 0 1, "lusab-babad" )
             , ( Ip 63 84 220 193, "gutih-tugad" )
             , ( Ip 63 118 7 35, "gutuk-bisog" )
             , ( Ip 140 98 193 141, "mudof-sakat" )
             , ( Ip 64 255 6 200, "haguz-biram" )
             , ( Ip 128 30 52 45, "mabiv-gibot" )
             , ( Ip 147 67 119 2, "natag-lisaf" )
             , ( Ip 212 58 253 68, "tibup-zujah" )
             , ( Ip 216 35 68 215, "tobog-higil" )
             , ( Ip 216 68 232 21, "todah-vobij" )
             , ( Ip 198 81 129 136, "sinid-makam" )
             , ( Ip 12 110 110 204, "budov-kuras" )
             ]
                |> List.concatMap
                    (\( Ip octet1 octet2 octet3 octet4, result ) ->
                        let
                            int =
                                combineOctets octet1 octet2
                                    * (2 ^ 16)
                                    -- 16-> 32 bit numbers break stuff
                                    + combineOctets octet3 octet4
                        in
                        [ test ("converts to " ++ result) <|
                            \_ ->
                                Proquint.fromInt int
                                    |> Maybe.map Proquint.toString
                                    |> Expect.equal (Just result)
                        , test ("converts from " ++ result) <|
                            \_ ->
                                Proquint.fromString result
                                    |> Maybe.map Proquint.toInt
                                    |> Expect.equal (Just int)
                        ]
                    )
            )
        ]


type Ip
    = Ip Int Int Int Int


combineOctets : Int -> Int -> Int
combineOctets first second =
    Bitwise.shiftLeftBy 8 first
        |> Bitwise.or second
