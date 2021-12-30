module NestedTested exposing (..)

import Accessors exposing (Relation, get, makeOneToOne, over, set)
import Accessors.Library exposing (onEach, try)
import Debug
import Dict exposing (Dict)
import Expect
import FuncAccessors exposing (..)
import Test exposing (Test, describe, test)


dictEntry : comparable -> Relation (Maybe v) sub wrap -> Relation (Dict comparable v) sub wrap
dictEntry key =
    makeOneToOne (Dict.get key) (Dict.update key)


type SomeMultiStateEnum
    = Nope
    | NuhUh
    | Yes (Dict String Int)


multiStateDict : Relation (Dict String Int) reachable a -> Relation SomeMultiStateEnum reachable (Maybe a)
multiStateDict =
    let
        get f super =
            case super of
                Yes dict ->
                    Just <| f dict

                _ ->
                    Nothing

        set f super =
            case super of
                Yes dict ->
                    Yes <| f dict

                _ ->
                    super
    in
    Accessors.makeOneToN get set

-- multiStateKey : String -> Relation (Maybe Int) sub wrap -> Relation SomeMultiStateEnum sub (Maybe wrap)
multiStateKey : String -> Relation Int sub wrap -> Relation SomeMultiStateEnum sub (Maybe (Maybe wrap))
multiStateKey key = multiStateDict << dictEntry key << try
