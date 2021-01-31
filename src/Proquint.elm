module Proquint exposing (Proquint, fromInt, fromString, toInt, toString)

{-| Basic usage:

    Proquint.fromInt 1234
        |> Maybe.map Proquint.toString

    Proquint.fromString "bonam-hohah"
        |> Maybe.map Proquint.toInt

    # Create
    @docs fromInt, fromString

    # Conversion
    @docs toInt, toString

    # Type
    @docs Proquint

-}

import Bitwise
import Parser exposing (..)


{-| Opaque type with a proquint inside
-}
type Proquint
    = Proquint Byte Byte Byte Byte


{-| Accepts any int between 0 and 0xFFFFFFFF
-}
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
        Just (Proquint a b c d)


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


{-| returns the int value of the proquint
-}
toInt : Proquint -> Int
toInt (Proquint a b c d) =
    bytesToInt a b
        -- bitshifting by 16 causes the sign to flip!
        |> Bitwise.shiftLeftBy 15
        |> (*) 2
        |> (+) (bytesToInt c d)


{-| parses proquint-strings to Proquints
-}
fromString : String -> Maybe Proquint
fromString input =
    Parser.run proquintParser input
        |> Result.toMaybe


type TwoBits
    = Zero
    | One
    | Two
    | Three -- we use two bits to simplify casing :-/


type HalfByte
    = HalfByte TwoBits TwoBits


type Byte
    = Byte HalfByte HalfByte


{-| returns the string value of the proquint
-}
toString : Proquint -> String
toString (Proquint a b c d) =
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


proquintParser : Parser Proquint
proquintParser =
    succeed buildProquint
        |= chunk
        |. symbol "-"
        |= chunk
        |. end


buildProquint : ( Byte, Byte ) -> ( Byte, Byte ) -> Proquint
buildProquint ( a, b ) ( c, d ) =
    Proquint a b c d


bytesToInt : Byte -> Byte -> Int
bytesToInt a b =
    byteToInt a
        |> Bitwise.shiftLeftBy 8
        |> Bitwise.or (byteToInt b)


byteToInt : Byte -> Int
byteToInt (Byte a b) =
    halfByteToInt a
        |> Bitwise.shiftLeftBy 4
        |> Bitwise.or (halfByteToInt b)


halfByteToInt : HalfByte -> Int
halfByteToInt (HalfByte a b) =
    twoBitsToInt a
        |> Bitwise.shiftLeftBy 2
        |> Bitwise.or (twoBitsToInt b)


twoBitsToInt : TwoBits -> Int
twoBitsToInt a =
    case a of
        Zero ->
            0

        One ->
            1

        Two ->
            2

        Three ->
            3


chunk : Parser ( Byte, Byte )
chunk =
    succeed combine
        |= consonant
        |= vowel
        |= consonant
        |= vowel
        |= consonant


combine : HalfByte -> TwoBits -> HalfByte -> TwoBits -> HalfByte -> ( Byte, Byte )
combine a b1 (HalfByte b2 c1) c2 d =
    ( Byte a (HalfByte b1 b2)
    , Byte (HalfByte c1 c2) d
    )


vowel : Parser TwoBits
vowel =
    oneOf
        [ succeed Zero
            |. symbol "a"
        , succeed One
            |. symbol "i"
        , succeed Two
            |. symbol "o"
        , succeed Three
            |. symbol "u"
        ]


consonant : Parser HalfByte
consonant =
    oneOf
        [ succeed (HalfByte Zero Zero)
            |. symbol "b"
        , succeed (HalfByte Zero One)
            |. symbol "d"
        , succeed (HalfByte Zero Two)
            |. symbol "f"
        , succeed (HalfByte Zero Three)
            |. symbol "g"
        , succeed (HalfByte One Zero)
            |. symbol "h"
        , succeed (HalfByte One One)
            |. symbol "j"
        , succeed (HalfByte One Two)
            |. symbol "k"
        , succeed (HalfByte One Three)
            |. symbol "l"
        , succeed (HalfByte Two Zero)
            |. symbol "m"
        , succeed (HalfByte Two One)
            |. symbol "n"
        , succeed (HalfByte Two Two)
            |. symbol "p"
        , succeed (HalfByte Two Three)
            |. symbol "r"
        , succeed (HalfByte Three Zero)
            |. symbol "s"
        , succeed (HalfByte Three One)
            |. symbol "t"
        , succeed (HalfByte Three Two)
            |. symbol "v"
        , succeed (HalfByte Three Three)
            |. symbol "z"
        ]
