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

module Pylon.DB.Mirror
  ( Mirror
  , mirror
  , refs
  , changedRefs
  , deltas
  , commit
  , refresh
  , attach
  , forward
  ) where

{-| A binding for ElmTextSearch and Pylon.DB.Group.

# Types
@docs Mirror

# Construction
@docs mirror

# Getters
@docs refs, changedRefs, deltas

# Group Mirroring
@docs refresh, attach, forward

# Integration
@docs commit
-}

import Pylon.DB as DB
import Pylon.DB.Group as DB

import Pylon.Resource as Resource exposing (Resource)

import Dict exposing (Dict)
import Set exposing (Set)


type alias MirrorState_ doctype =
  { resultRefs  : Dict String doctype
  , resultRefs_ : Dict String doctype

  , deltas : Dict String (List (Resource DB.DBError doctype, Resource DB.DBError doctype))
  , refresh : Set String
  }


{-| A document mirror. -}
type Mirror doctype =
  MirrorState (MirrorState_ doctype)


newState__ : MirrorState_ doctype
newState__ =
  { resultRefs = Dict.empty
  , resultRefs_ = Dict.empty

  , deltas = Dict.empty
  , refresh = Set.empty
  }


{-| Create a new mirror from a document description. -}
mirror : Mirror doctype
mirror =
  MirrorState newState__


{-| Get the current reference dictionary. -}
refs : Mirror doctype -> Dict String doctype
refs (MirrorState priorState) =
  priorState.resultRefs


{-| Get the pending reference dictionary. -}
changedRefs : Mirror doctype -> Dict String doctype
changedRefs (MirrorState priorState) =
  priorState.resultRefs_


{-| Get the deltas used to change the reference dictionary. -}
deltas : Mirror doctype -> Dict String (List (Resource DB.DBError doctype, Resource DB.DBError doctype))
deltas (MirrorState priorState) =
  priorState.deltas


{-| Forcibly refresh the mirrored data for a particular key. -}
refresh : List String -> Mirror doctype -> Mirror doctype
refresh keys priorShell =
  List.foldl
    (\key (MirrorState state as shell) ->
      if Dict.member key state.resultRefs then
        MirrorState { state | refresh = Set.insert key state.refresh }
      else
        shell
    ) priorShell keys


{-| Accept the current changes. -}
commit : Mirror doctype -> Mirror doctype
commit (MirrorState priorState as priorShell) =
  if Dict.isEmpty priorState.deltas then
    priorShell
  else
    MirrorState { priorState | deltas = Dict.empty, resultRefs = priorState.resultRefs_ }


{-| Mirror a DB group. -}
attach : (String -> rectype -> doctype) -> DB.Group (DB.Data rectype) -> Mirror doctype -> Mirror doctype
attach mirror group (MirrorState priorState as priorShell) =
  let
    toDoc key dat whc = Resource.therefore (mirror key) (whc dat)
    docPair key dat = toDoc key dat |> \f -> (f fst, f snd)

    currentData key =
      DB.getGroupSubData key group
      |> Resource.therefore (mirror key)

    priorRes key =
      Dict.get key priorState.resultRefs
      |> Maybe.map Resource.def
      |> Maybe.withDefault (currentData key)


    (MirrorState state as shell, refreshSet) =
      DB.groupDataResDeltaFoldR
        (\key dat (currentShell, refreshSet') ->
          ( docPair key dat
            |> flip (mirrorDelta__ key) currentShell
          , Set.remove key refreshSet'
          )
        )
        (priorShell, priorState.refresh)
        group


    -- execute manual refresh on elements whose mirror result will change given the current function
    (MirrorState state' as shell') =
      Set.foldl
        (\key (MirrorState currentState as currentShell) -> currentData key
        |> Resource.therefore (Resource.def >> (,) (priorRes key))
        |> Resource.therefore (flip (mirrorDelta__ key) currentShell)
        |> Resource.otherwise currentShell)
        shell
        refreshSet

  in
    if Set.isEmpty state'.refresh then
      shell'
    else
      MirrorState { state' | refresh = Set.empty }


{-| Forward deltas from one mirror to another. -}
forward : (String -> doctype -> doctype') -> Mirror doctype -> Mirror doctype' -> Mirror doctype'
forward mirror (MirrorState sourceState as sourceShell) (MirrorState priorState as priorShell) =
  let
    toDoc key dat whc = Resource.therefore (mirror key) (whc dat)
    docPair key dat = toDoc key dat |> \f -> (f fst, f snd)

  in
    Dict.foldr
      (\key -> flip (List.foldr (docPair key >> mirrorDelta__ key)))
      priorShell
      sourceState.deltas


updateMirrorDeltas__ : String -> (Resource DB.DBError doctype, Resource DB.DBError doctype) -> Mirror doctype -> Mirror doctype
updateMirrorDeltas__ key deltaPair (MirrorState priorState) =
  MirrorState
    { priorState
    | deltas = priorState.deltas
      |> Dict.get key
      |> Maybe.withDefault []
      |> (::) deltaPair
      |> flip (Dict.insert key) priorState.deltas
    }


mirrorDelta__ : String -> (Resource DB.DBError doctype, Resource DB.DBError doctype) -> Mirror doctype -> Mirror doctype
mirrorDelta__ key deltaPair (MirrorState priorState as priorShell) =
  case deltaPair of
    (Resource.Known doc0, Resource.Known doc1) ->
      MirrorState
        { priorState
        | resultRefs_ = priorState.resultRefs_
            |> Dict.remove key
            |> Dict.insert key doc1
        }
      |> updateMirrorDeltas__ key deltaPair

    (_, Resource.Known doc) ->
      MirrorState
        { priorState
        | resultRefs_ = Dict.insert key doc priorState.resultRefs_
        }
      |> updateMirrorDeltas__ key deltaPair

    (Resource.Known doc, _) ->
      MirrorState
        { priorState
        | resultRefs_ = Dict.remove key priorState.resultRefs_
        }
      |> updateMirrorDeltas__ key deltaPair

    _ ->
      priorShell
