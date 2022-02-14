module LicensePlate exposing (LicensePlate, empty, generator, letters, toString)

import Random


type LicensePlate
    = LicensePlate Char Char Char Int Int Int
    | Empty


empty : LicensePlate
empty =
    Empty


create : ( Char, Char, Char ) -> ( Int, Int, Int ) -> LicensePlate
create ( first, second, third ) ( fourth, fifth, sixth ) =
    LicensePlate first second third fourth fifth sixth


letters : LicensePlate -> String
letters plate =
    plate
        |> toString
        |> String.filter Char.isAlpha


toString : LicensePlate -> String
toString plate =
    case plate of
        Empty ->
            ""

        LicensePlate first second third fourth fifth sixth ->
            String.fromChar first
                ++ String.fromChar second
                ++ String.fromChar third
                ++ " "
                ++ String.fromInt fourth
                ++ String.fromInt fifth
                ++ String.fromInt sixth


digitGenerator : Random.Generator Int
digitGenerator =
    Random.int 0 9


charGenerator : Random.Generator Char
charGenerator =
    Random.map (\n -> Char.fromCode (n + 65)) (Random.int 0 25)


triple : a -> a -> a -> ( a, a, a )
triple first second third =
    ( first, second, third )


digitsGenerator : Random.Generator ( Int, Int, Int )
digitsGenerator =
    Random.map3 triple digitGenerator digitGenerator digitGenerator


lettersGenerator : Random.Generator ( Char, Char, Char )
lettersGenerator =
    Random.map3 triple charGenerator charGenerator charGenerator


generator : Random.Generator LicensePlate
generator =
    Random.map2 create lettersGenerator digitsGenerator
