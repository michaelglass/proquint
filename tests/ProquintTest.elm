module ProquintTest exposing (suite)

import Debug
import Expect
import Proquint exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "Proquint"
        [ describe "fromInt: Int -> Maybe Proxint"
            [ test "for an negative number, returns Nothing" <|
                \_ ->
                    Proquint.fromInt -1
                        |> Expect.equal Nothing
            , test "for an number > 32 bits, returns Nothing" <|
                \_ ->
                    Proquint.fromInt (0xFFFFFFFF + 1)
                        |> Expect.equal Nothing
            , test "for a valid int, returns something" <|
                \_ ->
                    Proquint.fromInt 0xFFFFFFFF
                        |> Expect.equal
                            (Just
                                (Proquint.Proquint 0xFFFFFFFF
                                    (Byte (HalfByte Three Three) (HalfByte Three Three))
                                    (Byte (HalfByte Three Three) (HalfByte Three Three))
                                    (Byte (HalfByte Three Three) (HalfByte Three Three))
                                    (Byte (HalfByte Three Three) (HalfByte Three Three))
                                )
                            )
            ]
        , describe "fromString : String -> Maybe Proxint"
            [ todo "for an invalid string, returns Nothing"
            , todo "for a valid string, returns something"
            ]
        , describe "toString : Proxint -> String"
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
                |> List.map
                    (\( Ip octet1 octet2 octet3 octet4, result ) ->
                        let
                            int =
                                octet1
                                    |> combineOctets octet2
                                    |> combineOctets octet3
                                    |> combineOctets octet4
                        in
                        test ("converts to " ++ result) <|
                            \_ ->
                                Proquint.fromInt int
                                    |> Maybe.map Proquint.toString
                                    |> Expect.equal (Just result)
                    )
            )
        ]


type Ip
    = Ip Int Int Int Int


combineOctets : Int -> Int -> Int
combineOctets second first =
    (first * (2 ^ 8)) + second
