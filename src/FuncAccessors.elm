module FuncAccessors exposing (..)

import Accessors exposing (Relation, get, set)


{-| Run a function over a relation's sub/wrap value, returning some value that can be mapped over in order to set the value within `super`.
For example, you could try to modify a deeply nested value, but decide the current value is unacceptable:

    import Accessors

    type alias NestedFoo = { foo : { foo: Int}}
    nestedZero = { foo = { foo = 0 }}
    nestedOne = { foo = { foo = 1 }}
    recordFooRelation = Accessors.makeOneToOne .foo <| \chg super -> { super | foo = chg super.foo }

    tryToDouble : Int -> Result String Int
    tryToDouble num = if num == 0 then (Err "Not doubling a zero") else (Ok <| num * 2)

    tryToDoubleNested : NestedFoo -> Result String NestedFoo
    tryToDoubleNested = overFunc (recordFooRelation << recordFooRelation) Result.map tryToDouble

    tryToDoubleNested nestedOne
    --> Ok { foo = { foo = 2}}

    tryToDoubleNested nestedZero
    --> Err "Not doubling a zero"

This does not work with relations that use `try`, for good reason. See `overTry` for that, and the reason why.

-}
overFunc : (Relation sub sub sub -> Relation super sub sub) -> ((sub -> super) -> subresult -> superresult) -> (sub -> subresult) -> super -> superresult
overFunc relation mapper f super =
    f (get relation super) |> mapper (\new -> set relation new super)


{-| Run a function over a relation's sub value, specifically when `try` was used.

    import Accessors
    import Accessors.Library exposing (try)

    type alias NestedFoo = { foo : Maybe { foo: Int}}
    nestedZero = { foo = Just { foo = 0 }}
    nestedOne = { foo = Just { foo = 1 }}
    nestedNothing = { foo = Nothing }
    recordFooRelation = Accessors.makeOneToOne .foo <| \chg super -> { super | foo = chg super.foo }

    tryToDouble : Int -> Result String Int
    tryToDouble num = if num == 0 then (Err "Not doubling a zero") else (Ok <| num * 2)

    tryToDoubleNested : NestedFoo -> Maybe (Result String NestedFoo)
    tryToDoubleNested = overTry (recordFooRelation << try << recordFooRelation) Result.map tryToDouble

    tryToDoubleNested nestedOne
    --> Just <| Ok { foo = Just { foo = 2}}

    tryToDoubleNested nestedZero
    --> Just <| Err "Not doubling a zero"

    tryToDoubleNested nestedNothing
    --> Nothing

This is separate from `overFunc` to avoid confusing behavior. Since we can only `get` the `wrap` from a relation, but `set` needs a `sub`,
this might imply that returning a "good" value will set a `Nothing` to `Just`. But that doesn't work, and depending upon the nested structure,
wouldn't even be possible in the first place. See `demonstrateWhyTryIsDifferent` in the unit tests to see what I mean.

-}
overTry : (Relation sub sub sub -> Relation super sub (Maybe sub)) -> ((sub -> super) -> subresult -> superresult) -> (sub -> subresult) -> super -> Maybe superresult
overTry relation mapper f super =
    Maybe.map f (get relation super) |> Maybe.map (mapper (\new -> set relation new super))


{-| Run a fallible function over a relation's sub/wrap value. Merely `overFunc` with `Result.map`, here for convenience.
-}
overFallible : (Relation sub sub sub -> Relation super sub sub) -> (sub -> Result e sub) -> super -> Result e super
overFallible relation =
    overFunc relation Result.map
