{--

Copyright (c) 2016, William Whitacre
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted
provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this list of conditions
and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions
and the following disclaimer in the documentation and/or other materials provided with the
distribution.

3. Neither the name of the copyright holder nor the names of its contributors may be used to endorse
or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

--}

module Pylon.Resource

  ( Resource(..)

  , def, pending, undecided, unknown, void

  , maybeOr, resultOr

  , maybeKnown

  , assumeIf, assumeIfNot, assumeInCase, deriveIf, deriveKnown

  , otherwise
  , therefore
  , decideBy

  , isUnknown, isNotUnknown
  , isPending, isNotPending
  , isUndecided, isNotUndecided
  , isVoid, isNotVoid
  , isKnown, isNotKnown
  , isAny, isAll

  ) where

{-| Resource types and reductions. Heavily pruned from Scaffold predecessor to give only what has
been tested, used in practice, and proven useful.

# Resource Type

This type represents a single remote resource in such a way that it's state can be easily reduced
to a more abstract representation, either for another part of the system, or the user's view itself.

We export the tags as well so as to allow the user easy case destructuring.
@docs Resource


# Resource Constructors
@docs def, pending, undecided, void, unknown


# Predicate Composition
@docs isAll, isAny


# Predicates
@docs isKnown, isNotKnown, isPending, isNotPending, isNotUndecided, isNotUnknown, isNotVoid, isUndecided, isUnknown, isVoid


# Reduction
@docs therefore, otherwise, decideBy, assumeIf, assumeIfNot, assumeInCase, deriveIf, deriveKnown


# Interop with Result and Maybe
@docs maybeKnown, maybeOr, resultOr

-}


import Debug


{-| A resource item. -}
type Resource errortype v =
  Unknown
  | Pending
  | Void
  | Undecided (errortype)
  | Known v


{-| True if the resource is unknown. -}
isUnknown : Resource errortype v -> Bool
isUnknown res =
  case res of
    Unknown -> True
    _ -> False


{-| False if the resource is unknown. -}
isNotUnknown : Resource errortype v -> Bool
isNotUnknown = isUnknown >> not


{-| True if the resource is pending. -}
isPending : Resource errortype v -> Bool
isPending res =
  case res of
    Pending -> True
    _ -> False

{-| False if the resource is pending. -}
isNotPending : Resource errortype v -> Bool
isNotPending = isPending >> not


{-| True if the resource is void. -}
isVoid : Resource errortype v -> Bool
isVoid res =
  case res of
    Void -> True
    _ -> False


{-| False if the resource is void. -}
isNotVoid : Resource errortype v -> Bool
isNotVoid = isVoid >> not


{-| True if the resource is undecided. -}
isUndecided : Resource errortype v -> Bool
isUndecided res =
  case res of
    Undecided _ -> True
    _ -> False


{-| False if the resource is undecided. -}
isNotUndecided : Resource errortype v -> Bool
isNotUndecided = isUndecided >> not


{-| True if the resource is known. -}
isKnown : Resource errortype v -> Bool
isKnown res =
  case res of
    Known _ -> True
    _ -> False


{-| False if the resource is known. -}
isNotKnown : Resource errortype v -> Bool
isNotKnown = isKnown >> not


{-| True if _any_ of the given predicates is True, otherwise False. -}
isAny : List (Resource errortype v -> Bool) -> Resource errortype v -> Bool
isAny predicates res =
  case predicates of
    [] -> Debug.crash "Called Resource.isAny with an empty list!"
    f :: [] -> f res
    f :: fs' -> if f res then True else isAny fs' res


{-| True if _all_ of the given predicates is True, otherwise False. -}
isAll : List (Resource errortype v -> Bool) -> Resource errortype v -> Bool
isAll predicates res =
  case predicates of
    [] -> Debug.crash "Called Resource.isAllOf with an empty list!"
    f :: [] -> f res
    f :: fs' -> if not (f res) then False else isAll fs' res


{-| Given a resource of value type v, create a resource of value type v' by transforming the
known value or group using some function (v -> v'). NOTE that this will create an entirely new
resouce structure, and thus any pending changes will be integrated immediately. If you wish to
preserve deltas for the purpose of mirroring and efficient data flow, then one should be using
deltaTo in order to transform just the changes. -}
therefore : (v -> v') -> Resource errortype v -> Resource errortype v'
therefore xdcr =
  deriveKnown (xdcr >> Known)


{-| If the resource is an undecided resource, then the given  -}
decideBy : (errortype -> Resource errortype v) -> Resource errortype v -> Resource errortype v
decideBy decider res =
  case res of
    Undecided err' -> decider err'
    _ -> res


{-| If the given predicate is satisfied, the result will be known to the the given value, otherwise
the given resource will be given unchanged. -}
assumeIf : (Resource errortype v -> Bool) -> v -> Resource errortype v -> Resource errortype v
assumeIf satisfies assume res =
  if satisfies res then Known assume else res


{-| Negation of assumeIf. -}
assumeIfNot : (Resource errortype v -> Bool) -> v -> Resource errortype v -> Resource errortype v
assumeIfNot satisfies assume res =
  assumeIf (satisfies >> not) assume res


{-| In the case that the provided function returns `Just x`, the resource shall be known as that
value, otherwise the Resource shall remain unchanged.  -}
assumeInCase : (Resource errortype v -> Maybe v) -> Resource errortype v -> Resource errortype v
assumeInCase possibleAssumption res =
  Maybe.map Known (possibleAssumption res)
  |> Maybe.withDefault res


{-| If the predicate is true, apply the given transformation to get a new resource. -}
deriveIf : (Resource errortype v' -> Bool) -> (Resource errortype v' -> Resource errortype v') -> Resource errortype v' -> Resource errortype v'
deriveIf satisfies f res =
  if satisfies res then f res else res


{-| If the resource is known, then use the given function to create an entirely new replacement resource,
otherwise carry the state to the new resource. This always transforms the type because no other states
carry the value type. -}
deriveKnown : (v -> Resource errortype v') -> Resource errortype v -> Resource errortype v'
deriveKnown xdcr res =
  case res of
    Unknown -> Unknown
    Pending -> Pending
    Void -> Void
    Undecided err' -> Undecided err'
    Known x' -> xdcr x'


{-| If a resource is known, then give Just it's value, otherwise Nothing. -}
maybeKnown : Resource errortype v' -> Maybe v'
maybeKnown res' =
  case res' of
    Known x' -> Just x'
    _ -> Nothing


{-| In the event that the given resource is not a simple `def`, we replace it with a different simple
resource. -}
otherwise : v' -> Resource errortype v' -> v'
otherwise assumption res' =
  case res' of
    Known x' -> x'
    _ -> assumption


{-|  -}
unknown : Resource errortype v
unknown = Unknown


{-|  -}
pending : Resource errortype v
pending = Pending


{-|  -}
void : Resource errortype v
void = Void


{-|  -}
undecided : errortype -> Resource errortype v
undecided = Undecided


{-|  -}
def : v -> Resource errortype v
def = Known


{-|  -}
resultOr : (errortype -> Resource errortype v) -> Result (errortype) v -> Resource errortype v
resultOr errorResource result =
  case result of
    Result.Ok data -> def data
    Result.Err err' -> errorResource err'


{-| -}
maybeOr : Resource errortype v -> Maybe v -> Resource errortype v
maybeOr nothingResource maybeValue =
  Maybe.map Known maybeValue
  |> Maybe.withDefault nothingResource
