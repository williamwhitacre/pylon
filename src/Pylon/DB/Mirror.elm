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
  , getRef
  , changedRefs
  , deltas
  , isEmpty
  , isChangedEmpty

  , each
  , refresh
  , resynch
  , attach
  , attachSynch
  , attachFilterSynch
  , attachDelta
  , attachFilterDelta

  , forward
  , forwardPast
  , filterForward
  , sort
  , filterSort
  , multiSort

  , inject
  , commit

  , bindMirror
  , bindMirrorRaw
  , groupMirror
  , groupMirrorSynch
  , dataGroupMirror
  , dataGroupMirrorSynch
  ) where

{-| A binding for ElmTextSearch and Pylon.DB.Group.

# Types
@docs Mirror

# Construction
@docs mirror

# Getters
@docs getRef, refs, changedRefs, deltas, isEmpty, isChangedEmpty

# Mirroring
@docs each, refresh, resynch, attach, attachSynch, attachDelta, attachFilterSynch, attachFilterDelta

# Dataflow
@docs forward, forwardPast, filterForward, sort, filterSort, multiSort

# Control
@docs inject, commit

# Group Binding
@docs bindMirror, bindMirrorRaw, groupMirror, groupMirror, groupMirrorSynch, dataGroupMirror, dataGroupMirrorSynch

-}

import Pylon.DB as DB
import Pylon.DB.Group as DB

import Pylon.Resource as Resource exposing (Resource)

import Dict exposing (Dict)
import Set exposing (Set)

import ElmFire


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


{-| -}
isEmpty : Mirror doctype -> Bool
isEmpty (MirrorState state) =
  Dict.isEmpty state.resultRefs


{-| -}
isChangedEmpty : Mirror doctype -> Bool
isChangedEmpty (MirrorState state) =
  Dict.isEmpty state.resultRefs_


{-| Get the current working reference dictionary. -}
refs : Mirror doctype -> Dict String doctype
refs (MirrorState priorState) =
  priorState.resultRefs


{-| Get the current working reference at a particular key. -}
getRef : String -> Mirror doctype -> Resource DB.DBError doctype
getRef key (MirrorState priorState) =
  Dict.get key priorState.resultRefs
  |> Maybe.map Resource.def
  |> Maybe.withDefault Resource.void


{-| Get the pending reference dictionary. -}
changedRefs : Mirror doctype -> Dict String doctype
changedRefs (MirrorState priorState) =
  priorState.resultRefs_


{-| Get the pending reference at a particular key. -}
getChangedRef : String -> Mirror doctype -> Resource DB.DBError doctype
getChangedRef key (MirrorState priorState) =
  Dict.get key priorState.resultRefs_
  |> Maybe.map Resource.def
  |> Maybe.withDefault Resource.void


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


{-| Emit all known values as deltas. This will cause any that forwards from this mirror and any
group that uses this mirror as a controller by deltas will be brought up to date. -}
resynch : Mirror doctype -> Mirror doctype
resynch (MirrorState priorState as priorShell) =
  Dict.foldr (\key -> Resource.def >> inject key) priorShell priorState.resultRefs_


{-| Do something to each member of a mirror. Useful for acting on nested mirrors, for example, deep resynch. -}
each : (doctype -> doctype) -> Mirror doctype -> Mirror doctype
each xdcr (MirrorState priorState as priorShell) =
  Dict.foldr (\key -> xdcr >> Resource.def >> inject key) priorShell priorState.resultRefs_


{-| Accept the current changes. -}
commit : Mirror doctype -> Mirror doctype
commit (MirrorState priorState as priorShell) =
  if Dict.isEmpty priorState.deltas then
    priorShell
  else
    MirrorState { priorState | deltas = Dict.empty, resultRefs = priorState.resultRefs_ }


{-| Inject a change. This can be used to manually control mirror sources in the case that they
are not derived from some DB.Group. -}
inject : String -> Resource DB.DBError doctype -> Mirror doctype -> Mirror doctype
inject key docResource priorShell =
  mirrorDelta__ key
    ( getChangedRef key priorShell
    , docResource
    ) priorShell


{-| Mirror a DB group. Depreciated alias for attachDelta. -}
attach : (String -> rectype -> doctype) -> DB.Group (DB.Data rectype) -> Mirror doctype -> Mirror doctype
attach =
  attachDelta

{-| Reflect the entire group working set in a mirror. -}
attachSynch : (String -> rectype -> doctype) -> DB.Group (DB.Data rectype) -> Mirror doctype -> Mirror doctype
attachSynch fmirror =
  attachSynch__ (\key -> fmirror key >> Just)


{-| Apply the currently pending set of group deltas to a mirror. -}
attachDelta : (String -> rectype -> doctype) -> DB.Group (DB.Data rectype) -> Mirror doctype -> Mirror doctype
attachDelta fmirror =
  attachDelta__ (\key -> fmirror key >> Just)


{-| Reflect the entire group working set in a mirror, excluding any key-record pairs that produce Nothing instead of Just a document. -}
attachFilterSynch : (String -> rectype -> Maybe doctype) -> DB.Group (DB.Data rectype) -> Mirror doctype -> Mirror doctype
attachFilterSynch =
  attachSynch__


{-| Apply the currently pending set of group deltas to a mirror, excluding any key-record pairs that produce Nothing instead of Just a document. -}
attachFilterDelta : (String -> rectype -> Maybe doctype) -> DB.Group (DB.Data rectype) -> Mirror doctype -> Mirror doctype
attachFilterDelta =
  attachDelta__


{-| Forward deltas from one mirror to another. -}
forward : (String -> doctype -> doctype') -> Mirror doctype -> Mirror doctype' -> Mirror doctype'
forward mirror (MirrorState sourceState as sourceShell) (MirrorState priorState as priorShell) =
  let
    toDoc key = Resource.therefore (mirror key)
    docPair key (prior, current) = (toDoc key prior, toDoc key current)

  in
    Dict.foldr
      (\key -> flip (List.foldr (docPair key >> mirrorDelta__ key)))
      priorShell
      sourceState.deltas


{-| Forward deltas, accounting for the previous output for the same key. This allows something
conceptually similar to folding in to the past of the output mirror, so unlike forward and sort, it
represents a user defined stateful update of the output mirror. -}
forwardPast : (String -> doctype -> Maybe doctype' -> Maybe doctype') -> Mirror doctype -> Mirror doctype' -> Mirror doctype'
forwardPast pastMirror (MirrorState sourceState as sourceShell) (MirrorState priorState as priorShell) =
  let
    toDoc key prevOutput res =
      Debug.log "Mirror debug - toDoc - (key, prevOutput, resource)" (key, prevOutput, res) |> \_ ->
      Resource.therefore
        (flip (pastMirror key) prevOutput
        >> Maybe.map Resource.def
        >> Maybe.withDefault Resource.void)
        res
      |> Resource.otherwise Resource.void

  in
    Debug.log "Mirror debug - forwardPast - prior state" priorState |> \_ ->
    Debug.log "Mirror debug - forwardPast - source state" sourceState |> \_ ->
    Dict.foldr
      (\key pairs shell' ->
        List.foldr
          (\(_, next) shell_ ->
            getChangedRef key shell_
            |> Resource.therefore Just
            |> Resource.otherwise Nothing
            |> Debug.log "Mirror debug - forwardPast - changedRef"
            |> flip (toDoc key) next
            |> flip (inject key) shell_
          )
          shell'
          pairs
      )
      priorShell
      sourceState.deltas
    |> Debug.log "Mirror debug - forwardPast - next state"


{-| Forward deltas from one mirror to another selectively by using a filter. -}
filterForward : (String -> doctype -> Maybe doctype') -> Mirror doctype -> Mirror doctype' -> Mirror doctype'
filterForward filterMirror (MirrorState sourceState as sourceShell) (MirrorState priorState as priorShell) =
  let
    toDoc key =
      Resource.therefore
        (filterMirror key
        >> Maybe.map Resource.def
        >> Maybe.withDefault Resource.void)
      >> Resource.otherwise Resource.void

    docPair key (prior, current) = (toDoc key prior, toDoc key current)

  in
    Dict.foldr
      (\key -> flip (List.foldr (docPair key >> mirrorDelta__ key)))
      priorShell
      sourceState.deltas


{-| Sort the documents in a mirror in to buckets. -}
sort : (String -> doctype -> String) -> Mirror doctype -> Mirror (Mirror doctype) -> Mirror (Mirror doctype)
sort fsort =
  filterSort (\key -> fsort key >> Just)


{-| Sort the documents in a mirror in to buckets, ignoring the items whose key/document pairs yield
Nothing. If fsort produces `Just key'`, then the document will be placed in to the mirror represeting
the bucket at `key'`. -}
filterSort : (String -> doctype -> Maybe String) -> Mirror doctype -> Mirror (Mirror doctype) -> Mirror (Mirror doctype)
filterSort fsort =
  multiSort (\key -> fsort key >> Maybe.map (\x -> [x]) >> Maybe.withDefault [])


{-| Sort the documents in a mirror in to any arbitrary number of buckets per document. The sorting
function will result in a list of strings. If the list is empty the document is ignored. If the list
is not empty, then the given document will be mirrored in the bucket mirrors at the given keys. -}
multiSort : (String -> doctype -> List String) -> Mirror doctype -> Mirror (Mirror doctype) -> Mirror (Mirror doctype)
multiSort fsort (MirrorState sourceState as sourceShell) (MirrorState priorState as priorShell) =
  Dict.foldr
    (\key ->
      let
        -- : Mirror (Mirror (doctype))
        removeFromPriorBucket priorKey shell_ =
          let priorKeyBucket = getChangedRef priorKey shell_ in
            if Resource.isKnown priorKeyBucket then
              let
                priorKeyBucket' =
                  priorKeyBucket
                  |> Resource.therefore (inject key Resource.void)
                  |> Resource.deriveIf Resource.isKnown
                      (\bucketR ->
                        case bucketR of
                          Resource.Known bucket' ->
                            if isChangedEmpty bucket' then
                              Resource.void
                            else
                              Resource.def bucket'
                          _ ->
                            Resource.void)

              in
                inject priorKey priorKeyBucket' shell_

            else
              shell_

        -- : Resource DBError doctype -> Mirror (Mirror (doctype))
        editNextBucket res nextKey shell_ =
          if Resource.isKnown res then
            let
              nextKeyBucket =
                getChangedRef nextKey shell_
                |> Resource.otherwise mirror
            in
              inject
                nextKey
                (Resource.def <| inject key res nextKeyBucket)
                shell_
          else
            shell_

        removeFromPriorBuckets = flip <| List.foldl removeFromPriorBucket
        editNextBuckets res    = flip <| List.foldl (editNextBucket res)

        bucketKeysOf =
          Resource.therefore (fsort key) >> Resource.otherwise []
      in
        flip <| List.foldr
          (\(prior, next) ->
            removeFromPriorBuckets (bucketKeysOf prior)
            >> editNextBuckets next (bucketKeysOf next)
          )
    ) priorShell sourceState.deltas


{-|  -}
bindMirror
  :  (Signal.Address (List subfeedback) -> Maybe ElmFire.Location -> String -> doctype -> subbinding)
  -> Mirror doctype
  -> Signal.Address (List subfeedback)
  -> Maybe ElmFire.Location
  -> String
  -> subbinding
bindMirror route (MirrorState sourceState as sourceShell) address location key =
  case Dict.get key sourceState.resultRefs_ of
    Just ref -> route address location key ref
    Nothing -> Debug.crash ("Since the source document at " ++ key ++ " does not exist, bindMirror should never have been reached with this key.")


{-|  -}
bindMirrorRaw
  :  (DB.GroupConfig subfeedback subbinding -> String -> doctype -> subbinding)
  -> Mirror doctype
  -> DB.GroupConfig subfeedback subbinding
  -> String
  -> subbinding
bindMirrorRaw route (MirrorState sourceState as sourceShell) config key =
  case Dict.get key sourceState.resultRefs_ of
    Just ref -> route config key ref
    Nothing -> Debug.crash ("Since the source document at " ++ key ++ " does not exist, bindMirror should never have been reached with this key.")


{-| Use this in place of groupSubscription to synchronize the group's keys to a mirror. -}
groupMirror : subtype -> Mirror doctype -> DB.GroupConfig subfeedback subbinding -> DB.Group subtype -> (DB.Group subtype, List (DB.DBTask never))
groupMirror newSub (MirrorState sourceState as sourceShell) config group =
  Dict.foldr
    (\key -> flip
      (List.foldr
        (\(prior, curr) (group', tasks) ->
          case (prior, curr) of
            (Resource.Known _, Resource.Known _) ->
              ( DB.groupAddSub newSub key group'
                |> DB.groupUpdateSub identity key
              , Signal.send (DB.groupConfigGetAddress config) [DB.GroupRefresh key] :: tasks
              )
            (_, Resource.Known _) -> (DB.groupAddSub newSub key group', Signal.send (DB.groupConfigGetAddress config) [DB.GroupRefresh key] :: tasks)
            (Resource.Known _, _) -> (DB.groupRemoveSub key group', Signal.send (DB.groupConfigGetAddress config) [DB.GroupRefresh key] :: tasks)
            (_, _) -> (group', tasks)
        )
      )
    )
    (group, [])
    sourceState.deltas


{-| -}
dataGroupMirror : Mirror doctype -> DB.GroupConfig (DB.Feedback v) (DB.Binding v) -> DB.Group (DB.Data v) -> (DB.Group (DB.Data v), List (DB.DBTask never))
dataGroupMirror =
  groupMirror DB.newData


{-| Use this in place of groupSubscription to synchronize the group's keys to a mirror. -}
groupMirrorSynch : subtype -> (subtype -> (subtype, List (DB.DBTask never))) -> Mirror doctype -> DB.GroupConfig subfeedback subbinding -> DB.Group subtype -> (DB.Group subtype, List (DB.DBTask never))
groupMirrorSynch newSub cancelSub (MirrorState sourceState as sourceShell) config group =
  Dict.foldr
    (\key curr (group', tasks) ->
      (DB.groupAddSub newSub key group', Signal.send (DB.groupConfigGetAddress config) [DB.GroupRefresh key] :: tasks)
    )
    (DB.cancelAndResetGroup cancelSub group)
    sourceState.resultRefs_


{-| -}
dataGroupMirrorSynch : Mirror doctype -> DB.GroupConfig (DB.Feedback v) (DB.Binding v) -> DB.Group (DB.Data v) -> (DB.Group (DB.Data v), List (DB.DBTask never))
dataGroupMirrorSynch =
  groupMirrorSynch DB.newData DB.cancel



attachSynch__ : (String -> rectype -> Maybe doctype) -> DB.Group (DB.Data rectype) -> Mirror doctype -> Mirror doctype
attachSynch__ =
  attachSynchAny__ .value


attachDelta__ : (String -> rectype -> Maybe doctype) -> DB.Group (DB.Data rectype) -> Mirror doctype -> Mirror doctype
attachDelta__ =
  attachDeltaAny__ .value


attachSynchAny__ : (subtype -> Resource DB.DBError rectype) -> (String -> rectype -> Maybe doctype) -> DB.Group subtype -> Mirror doctype -> Mirror doctype
attachSynchAny__ fderive maybeMirror group (MirrorState priorState as priorShell) =
  let
    mirrorResource key =
      Resource.deriveKnown
        (maybeMirror key >> Maybe.map Resource.def >> Maybe.withDefault Resource.void)

    docPair key (prior, current) =
      (mirrorResource key prior, mirrorResource key current)

    getRes (MirrorState q) key =
      Dict.get key q.resultRefs_
      |> Maybe.map Resource.def
      |> Maybe.withDefault Resource.void


    rewriteDeltaPriors =
      priorState.resultRefs
      |> Dict.map (\_ _ -> Resource.void)
      |> Dict.union (Dict.map (always Resource.def) priorState.resultRefs_)


    (MirrorState state as shell) =
      Dict.foldl
        (\key resource -> mirrorDelta__ key (resource, Resource.void))
        priorShell
        rewriteDeltaPriors

  in
    Dict.foldl
      (\key sub shell' ->
        mirrorDelta__ key
          ( getRes shell' key
          , mirrorResource key (fderive sub)
          ) shell'
      ) shell (DB.getGroupNextData group)


attachDeltaAny__ : (subtype -> Resource DB.DBError rectype) -> (String -> rectype -> Maybe doctype) -> DB.Group subtype -> Mirror doctype -> Mirror doctype
attachDeltaAny__ fderive maybeMirror group (MirrorState priorState as priorShell) =
  let
    mirrorResource key =
      Resource.deriveKnown
        (maybeMirror key >> Maybe.map Resource.def >> Maybe.withDefault Resource.void)

    docPair key (prior, current) =
      (mirrorResource key prior, mirrorResource key current)

    currentData key =
      DB.getGroupSub key group
      |> Maybe.map fderive
      |> Maybe.withDefault Resource.void
      |> mirrorResource key


    priorRes key =
      Dict.get key priorState.resultRefs
      |> Maybe.map Resource.def
      |> Maybe.withDefault (currentData key)


    (MirrorState state as shell, refreshSet) =
      DB.groupDeriveDeltaFoldR
        fderive
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
