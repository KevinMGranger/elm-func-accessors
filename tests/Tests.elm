module Tests exposing (..)

import Accessors exposing (Relation, get, makeOneToOne, over, set)
import Accessors.Library exposing (onEach, try)
import Expect
import FuncAccessors exposing (..)
import Test exposing (Test, describe, test)


fooListFooNum : Relation Int reachable wrap -> Relation FooList reachable (List wrap)
fooListFooNum =
    listRecRel << onEach << fooNumFoo


flfnGet : FooList -> List Int
flfnGet =
    get fooListFooNum


type alias FooNum =
    { foo : Int }


type alias FooMaybe =
    { foo : Maybe FooNum }


type alias FooList =
    { foo : List FooNum }


fooNum1 =
    { foo = 1 }


fooNum0 =
    { foo = 0 }


nestRec : Int -> FooMaybe
nestRec n =
    { foo = Just { foo = n } }


listRec : List Int -> FooList
listRec l =
    FooList <| List.map FooNum l


listRecGoodNums : FooList
listRecGoodNums =
    listRec [ 1, 2, 3 ]


listRecRel : Relation (List FooNum) reachable wrap -> Relation FooList reachable wrap
listRecRel =
    makeOneToOne .foo <| \chg super -> { super | foo = chg super.foo }


listRecEach : Relation FooNum sub wrap -> Relation FooList sub (List wrap)
listRecEach =
    listRecRel << onEach


setty : (FooNum -> FooNum) -> FooList -> FooList
setty =
    over listRecEach


nestRec1 =
    nestRec 1


nestRec0 =
    nestRec 0


justNestRec =
    Just nestRec


nestRecNothing : FooMaybe
nestRecNothing =
    { foo = Nothing }


fooNumFoo : Relation Int reachable wrap -> Relation FooNum reachable wrap
fooNumFoo =
    let
        getter : FooNum -> Int
        getter =
            .foo

        setter : (Int -> Int) -> FooNum -> FooNum
        setter chg super =
            { super | foo = chg super.foo }
    in
    makeOneToOne getter setter


fooMaybeFoo : Relation (Maybe FooNum) reachable wrap -> Relation FooMaybe reachable wrap
fooMaybeFoo =
    let
        getter : FooMaybe -> Maybe FooNum
        getter =
            .foo

        setter : (Maybe FooNum -> Maybe FooNum) -> FooMaybe -> FooMaybe
        setter chg super =
            { super | foo = chg super.foo }
    in
    makeOneToOne getter setter


fooNumOver : ((Int -> FooNum) -> result -> superresult) -> (Int -> result) -> FooNum -> superresult
fooNumOver =
    overFunc fooNumFoo


fooMaybeOver : ((Maybe FooNum -> FooMaybe) -> result -> superresult) -> (Maybe FooNum -> result) -> FooMaybe -> superresult
fooMaybeOver =
    overFunc fooMaybeFoo


fooMaybeTry : Relation FooNum sub wrap -> Relation FooMaybe sub (Maybe wrap)
fooMaybeTry =
    fooMaybeFoo << try


tryMaybeFoo : Relation (Maybe FooNum) reachable wrap -> Relation (Maybe FooMaybe) reachable (Maybe wrap)
tryMaybeFoo =
    try << fooMaybeFoo


rightPath : Relation Int reachable wrap -> Relation FooMaybe reachable (Maybe wrap)
rightPath =
    fooMaybeFoo << try << fooNumFoo


zeroErr =
    Err "I can't handle zero!"


incIfPresent : number -> Result String number
incIfPresent num =
    if num /= 0 then
        Ok <| num + 1

    else
        zeroErr


changeOrNot : number -> Maybe number
changeOrNot num =
    if num /= 0 then
        Just <| num + 1

    else
        Nothing


basics : Test
basics =
    describe "basics"
        [ test "fooNumFoo 2 success" <| \_ -> fooNum1 |> overFallible fooNumFoo incIfPresent |> Expect.equal (Ok { foo = 2 })
        , test "fooNumFoo 0 fail" <| \_ -> fooNum0 |> overFallible fooNumFoo incIfPresent |> Expect.equal zeroErr
        , test "fooNumFoo 2 just" <| \_ -> fooNum1 |> overFunc fooNumFoo Maybe.map changeOrNot |> Expect.equal (Just { foo = 2 })
        , test "fooNumFoo 0 nothing" <| \_ -> fooNum0 |> overFunc fooNumFoo Maybe.map changeOrNot |> Expect.equal Nothing
        ]


tryingTry : Test
tryingTry =
    let
        over : ((Int -> FooMaybe) -> result -> superresult) -> (Int -> result) -> FooMaybe -> Maybe superresult
        over =
            overTry rightPath
    in
    describe "trying try"
        [ test "rightPath 2 success" <| \_ -> nestRec1 |> over Result.map incIfPresent |> Expect.equal (Just <| Ok { foo = Just { foo = 2 } })
        , test "rightPath 0 fail" <| \_ -> nestRec0 |> over Result.map incIfPresent |> Expect.equal (Just zeroErr)
        , test "rightPath nested just" <| \_ -> nestRec1 |> over Maybe.map changeOrNot |> Expect.equal (Just <| Just { foo = Just { foo = 2 } })
        , test "rightPath nested just fixed" <| \_ -> nestRec1 |> over Maybe.map changeOrNot |> Expect.equal (Just <| Just { foo = Just { foo = 2 } })
        , test "rightPath nested Nothing?" <| \_ -> nestRec0 |> over Maybe.map changeOrNot |> Expect.equal (Just Nothing)
        , test "rightPath nothing nested just" <| \_ -> nestRecNothing |> over Maybe.map changeOrNot |> Expect.equal Nothing
        , test "rightPath nothing success?" <| \_ -> nestRecNothing |> over Result.map incIfPresent |> Expect.equal Nothing
        , test "rightPath nothing fail" <| \_ -> nestRecNothing |> over Result.map incIfPresent |> Expect.equal Nothing
        ]


stateful : Test
stateful =
    let
        statefulOperation : Int -> ( Int, String )
        statefulOperation num =
            if num == 1 then
                ( num - 1, moreFoos )

            else
                ( num - 1, "" )

        moreFoos =
            "We're out of foos, please get more"
    in
    describe "stateful results (Tuples)"
        [ test "return some 'command'" <| \_ -> fooNum1 |> overFunc fooNumFoo Tuple.mapFirst statefulOperation |> Expect.equal ( { foo = 0 }, moreFoos )
        , test "this is fine" <| \_ -> fooNum0 |> overFunc fooNumFoo Tuple.mapFirst statefulOperation |> Expect.equal ( { foo = -1 }, "" )
        ]


{-| This function's signature is subtly different from `overFunc` in that the Relation is allowed to have different `wrap` and `sub` types.
That means that `try` isn't specially handled. The consequences of this are shown below.
-}
overFuncUnsafe : (Relation sub sub sub -> Relation super sub wrap) -> ((sub -> super) -> result -> superresult) -> (wrap -> result) -> super -> superresult
overFuncUnsafe relation mapper f super =
    f (get relation super) |> mapper (\new -> set relation new super)


demonstrateWhyTryIsDifferent : Test
demonstrateWhyTryIsDifferent =
    test "demonstrate why try needs special handling" <|
        \_ ->
            -- is this the value you expect to see?
            nestRecNothing |> overFuncUnsafe rightPath Result.map (always (Ok 3)) |> Expect.equal (Ok { foo = Nothing })
