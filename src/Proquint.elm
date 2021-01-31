module Proquint exposing (Byte(..), HalfByte(..), Proquint(..), TwoBits(..), fromInt, fromString, toInt, toString)

import Bitwise


type Proquint
    = Proquint Int Byte Byte Byte Byte


fromInt : Int -> Maybe Proquint
fromInt input =
    if input < 0 || input > 0xFFFFFFFF then
        Nothing

    else
        let
            ( a, b ) =
                input
                    |> Bitwise.and 0xFFFF0000
                    |> Bitwise.shiftRightZfBy 16
                    |> SixteenBits
                    |> toBytes

            ( c, d ) =
                input
                    |> Bitwise.and 0xFFFF
                    |> SixteenBits
                    |> toBytes
        in
        Just (Proquint input a b c d)


type SixteenBits
    = SixteenBits Int


toBytes : SixteenBits -> ( Byte, Byte )
toBytes input =
    let
        ( first, withoutFirstTwo ) =
            takeAByte input

        ( second, _ ) =
            takeAByte withoutFirstTwo
    in
    ( first, second )


takeAByte : SixteenBits -> ( Byte, SixteenBits )
takeAByte input =
    let
        ( firstHalf, withoutFirstTwo ) =
            takeHalfByte input

        ( secondHalf, remainder ) =
            takeHalfByte withoutFirstTwo
    in
    ( Byte firstHalf secondHalf, remainder )


takeHalfByte : SixteenBits -> ( HalfByte, SixteenBits )
takeHalfByte input =
    let
        ( firstTwo, withoutFirstTwo ) =
            takeTwoBits input

        ( secondTwo, remainder ) =
            takeTwoBits withoutFirstTwo
    in
    ( HalfByte firstTwo secondTwo, remainder )


takeTwoBits : SixteenBits -> ( TwoBits, SixteenBits )
takeTwoBits (SixteenBits input) =
    let
        leftTwoMask =
            0xC000

        allByLeftTwoMask =
            0x3FFF

        twoBits =
            input
                -- mask leftmost 2 bits
                |> Bitwise.and leftTwoMask
                |> Bitwise.shiftRightZfBy 14

        rest =
            input
                |> Bitwise.and allByLeftTwoMask
                |> Bitwise.shiftLeftBy 2

        -- can't bitshift because sometimes flips
    in
    ( case twoBits of
        0 ->
            Zero

        1 ->
            One

        2 ->
            Two

        3 ->
            Three

        _ ->
            Three
    , SixteenBits rest
    )


toInt : Proquint -> Int
toInt (Proquint input _ _ _ _) =
    input


fromString : String -> Maybe Proquint
fromString str =
    Debug.todo "get not yet!"
        Nothing


type TwoBits
    = Zero
    | One
    | Two
    | Three -- we use two bits to simplify casing :-/


type HalfByte
    = HalfByte TwoBits TwoBits


type Byte
    = Byte HalfByte HalfByte


toString : Proquint -> String
toString (Proquint _ a b c d) =
    twoBytesToSection a b ++ "-" ++ twoBytesToSection c d


twoBytesToSection : Byte -> Byte -> String
twoBytesToSection (Byte a (HalfByte b c)) (Byte (HalfByte d e) f) =
    String.fromList
        [ fourBitsToConsonant a
        , twoBitsToVowel b
        , fourBitsToConsonant (HalfByte c d)
        , twoBitsToVowel e
        , fourBitsToConsonant f
        ]


fourBitsToConsonant : HalfByte -> Char
fourBitsToConsonant (HalfByte firstHalf secondHalf) =
    case ( firstHalf, secondHalf ) of
        ( Zero, Zero ) ->
            'b'

        ( Zero, One ) ->
            'd'

        ( Zero, Two ) ->
            'f'

        ( Zero, Three ) ->
            'g'

        ( One, Zero ) ->
            'h'

        ( One, One ) ->
            'j'

        ( One, Two ) ->
            'k'

        ( One, Three ) ->
            'l'

        ( Two, Zero ) ->
            'm'

        ( Two, One ) ->
            'n'

        ( Two, Two ) ->
            'p'

        ( Two, Three ) ->
            'r'

        ( Three, Zero ) ->
            's'

        ( Three, One ) ->
            't'

        ( Three, Two ) ->
            'v'

        ( Three, Three ) ->
            'z'


twoBitsToVowel : TwoBits -> Char
twoBitsToVowel twoBits =
    case twoBits of
        Zero ->
            'a'

        One ->
            'i'

        Two ->
            'o'

        Three ->
            'u'
