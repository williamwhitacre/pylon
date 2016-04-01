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

module Pylon.DB.Filter
  ( Filter
  , FilterFeedback
  , Document

  , filter
  , filterQuery

  , getFilterQuery
  , getFilterResults
  , getLastFilterResults

  , filterMirror

  , filterInputOne
  , filterSync
  , filterAsyncAs
  , filterAsync
  ) where

{-| A binding for ElmTextSearch and Pylon.DB.Group.

# Types
@docs Filter, FilterFeedback, Document

# Construction
@docs filter, filterQuery

# Getters
@docs getFilterQuery, getFilterResults, getLastFilterResults

# Group Mirroring
@docs filterMirror

# Update
@docs filterInputOne

# Dispatch
@docs filterSync, filterAsyncAs, filterAsync
-}

import Pylon.App as App

import Pylon.DB as DB
import Pylon.DB.Group as DB

import Pylon.Resource as Resource exposing (Resource)

import Dict exposing (Dict)
import Task exposing (Task, andThen, onError)

import ElmTextSearch


type alias FilterState_ doctype =
  { resultRefs  : Dict String doctype
  , resultRefs_ : Dict String doctype

  , index  : ElmTextSearch.Index doctype
  , index_ : ElmTextSearch.Index doctype

  , query : (String, String)
  , results : Resource String (List (doctype, Float))
  , latest : (Int, List (doctype, Float))

  , indexChanged : Bool
  , sequenceNo : Int
  }


{-| A document filter. -}
type Filter doctype =
  FilterState
    (Document doctype)
    (FilterState_ doctype)


{-| FilterFeedback for a filter containing results. These will be matched to the current state by
sequence number, so old search results will not overwrite a newer query. -}
type FilterFeedback doctype =
  SearchComplete (Filter doctype)


{-| Represents the getters for a document type used by a filter. -}
type alias Document doctype =
  { ref : doctype -> String
  , fields : List (doctype -> String, Float)
  }


innerConfig__ : Document doctype -> ElmTextSearch.SimpleConfig doctype
innerConfig__ config =
  { ref = config.ref
  , fields = config.fields
  }


newState__ : Document doctype -> FilterState_ doctype
newState__ config =
  let
    index = ElmTextSearch.new (innerConfig__ config)

  in
    { resultRefs = Dict.empty
    , resultRefs_ = Dict.empty

    , index = index
    , index_ = index

    , query = ("", "")
    , results = Resource.void
    , latest = (-1, [])

    , indexChanged = False
    , sequenceNo = 0
    }


{-| Create a new filter from a document description. -}
filter : Document doctype -> Filter doctype
filter config =
  FilterState config (newState__ config)


{-| Set a new query string. -}
filterQuery : String -> Filter doctype -> Filter doctype
filterQuery query (FilterState filterConfig priorState as priorShell) =
  if query /= snd priorState.query then
    FilterState filterConfig { priorState | query = (fst priorState.query, query) }
  else
    priorShell


{-| Get the latest filter query. -}
getFilterQuery : Filter doctype -> String
getFilterQuery (FilterState filterConfig priorState) =
  snd priorState.query


{-| Get the current filter results. -}
getFilterResults : Filter doctype -> Resource String (List (doctype, Float))
getFilterResults (FilterState filterConfig priorState) =
  priorState.results


{-| Get the latest filter results. -}
getLastFilterResults : Filter doctype -> Maybe (List (doctype, Float))
getLastFilterResults (FilterState filterConfig priorState) =
  let
    (seqNo, list) = priorState.latest

  in
    if seqNo > -1 then Just list else Nothing


{-| Synchronize the document store with the contents of a DB group. -}
filterMirror : (String -> rectype -> doctype) -> DB.Group (DB.Data rectype) -> Filter doctype -> Filter doctype
filterMirror mirror group (FilterState filterConfig priorState as priorShell) =
  let
    toDoc key dat whc = Resource.therefore (mirror key) (whc dat)
    docPair key dat = toDoc key dat |> \f -> (f snd, f fst)

  in
    DB.groupDataResDeltaFoldR
      (\key -> docPair key >> filterDelta__ key)
      priorShell
      group


{-| Handle a filter result. This may update the latest field to reflect the latest results
continuously, even if the sequence numbers do not match. If they do, the existing filter will be
entirely replaced by the result one since the index cannot have changed since then. -}
filterInputOne : FilterFeedback doctype -> Filter doctype -> Filter doctype
filterInputOne (SearchComplete (FilterState _ s' as gs')) (FilterState config s as gs) =
  if s'.sequenceNo == s.sequenceNo then
    gs'
  else if s'.sequenceNo < s.sequenceNo && fst s'.latest > fst s.latest then
    FilterState config { s | latest = s'.latest }
  else
    gs


filterRunStage__ : Filter doctype -> (Filter doctype, Maybe (() -> Filter doctype))
filterRunStage__ (FilterState config priorState as priorShell) =
  let
    currentQuery =
      snd priorState.query

    changedQuery =
      fst priorState.query /= currentQuery

    changedWorkingSet =
      priorState.indexChanged

    changedIfWork bw s' =
      if bw then
        { s'
        | index = priorState.index_
        , resultRefs = priorState.resultRefs_
        , indexChanged = False
        } else s'

    changedIfQuery bq s' =
      if bq then { s' | query = (currentQuery, currentQuery) } else s'

    updateSequence bu s' =
      if bu then { s' | sequenceNo = 1 + s'.sequenceNo, results = Resource.pending } else s'

    intermediate chwork chquery =
      changedIfWork chwork
      >> changedIfQuery chquery
      >> updateSequence (chquery || chwork)

    requiresAction =
      changedQuery || changedWorkingSet
  in
    if requiresAction then
      let
        filterResultOf (key, rank) =
          Dict.get key im.resultRefs
          |> Maybe.map (\doc -> (doc, rank))

        im = intermediate changedWorkingSet changedQuery priorState

        computeResult _ =
          ElmTextSearch.search currentQuery priorState.index_
          |> Result.map
              (\(optimalIndex, results) ->
                List.filterMap filterResultOf results
                |> \processed ->
                    { im
                    | results = Resource.def processed
                    , latest = (im.sequenceNo, processed)
                    , index = optimalIndex
                    })
          |> Result.formatError
              (\error -> { im | results = Resource.undecided error })
          |> (\result -> case result of
                Result.Err badstate -> badstate
                Result.Ok okstate -> okstate)
          |> FilterState config

      in
        (FilterState config im, Just computeResult)
    else
      (priorShell, Nothing)


{-| Filter synchronously. The results are immediately available in the resulting Filter. -}
filterSync : Filter doctype -> Filter doctype
filterSync priorShell =
  filterRunStage__ priorShell
  |> \results -> case results of
      (_, Just computeResult) -> computeResult ()
      (shell, Nothing) -> shell


{-| Filter asynchronously with a forwarding address. -}
filterAsyncAs : (List (FilterFeedback doctype) -> List action) -> Signal.Address (List action) -> Filter doctype -> (Filter doctype, List (App.FinalTask never))
filterAsyncAs faction address =
  filterAsync (Signal.forwardTo address faction)


{-| Filter asynchronously. When executed, the resulting task will send update(s) to the provided filter feedback address. -}
filterAsync : Signal.Address (List (FilterFeedback doctype)) -> Filter doctype -> (Filter doctype, List (App.FinalTask never))
filterAsync address priorShell =
  let
    (shell, maybeDeferred) =
      filterRunStage__ priorShell

  in
    Maybe.map
      (\computeResult -> Task.succeed ()
        `andThen` (computeResult >> SearchComplete >> flip (::) [] >> Signal.send address)
        `andThen` (\_ -> Task.succeed ())
        `onError` (\_ -> Task.succeed ()))
      maybeDeferred
    |> Maybe.map (\task -> (shell, [ task ]))
    |> Maybe.withDefault (shell, [ ])



filterDelta__ : String -> (Resource DB.DBError doctype, Resource DB.DBError doctype) -> Filter doctype -> Filter doctype
filterDelta__ key (priorDocRes, currDocRes) (FilterState config priorState as priorShell) =
  let
    resultRefs = priorState.resultRefs_
    index = priorState.index_

    maybeIndex' =
      case (priorDocRes, currDocRes) of
        (Resource.Known doc0, Resource.Known doc1) ->
          Debug.log "Replacing Document" (doc0, doc1)
          |> \_ -> ElmTextSearch.remove doc0 index
          |> flip Result.andThen (ElmTextSearch.add doc1)
          |> Result.map (\index_ -> Debug.log "Replaced Document" (doc0, doc1) |> \_ -> index_)
          |> Result.formatError ((,) (doc0, doc1) >> Debug.log "Failed Replaing Document")
          |> Result.toMaybe

        (_, Resource.Known doc) ->
          ElmTextSearch.add (Debug.log "Adding Document" doc) index
          |> Result.map (\index_ -> Debug.log "Added Document" doc |> \_ -> index_)
          |> Result.formatError ((,) doc >> Debug.log "Failed Adding Document")
          |> Result.toMaybe

        (Resource.Known doc, _) ->
          ElmTextSearch.remove (Debug.log "Removing Document" doc) index
          |> Result.map (\index_ -> Debug.log "Removed Document" doc |> \_ -> index_)
          |> Result.formatError ((,) doc >> Debug.log "Failed Removing Document")
          |> Result.toMaybe

        _ -> Nothing

  in
    maybeIndex'
    |> Maybe.map
      (\index' ->
        { priorState | index_ = index', indexChanged = True, sequenceNo = 1 + priorState.sequenceNo }
        |> \im -> case (priorDocRes, currDocRes) of
          (Resource.Known doc0, Resource.Known doc1) ->
            FilterState config
              { im
              | resultRefs_ = resultRefs
                  |> Dict.remove (config.ref doc0)
                  |> Dict.insert (config.ref doc1) doc1
              }

          (_, Resource.Known doc) ->
            FilterState config { im | resultRefs_ = Dict.insert (config.ref doc) doc resultRefs }

          (Resource.Known doc, _) ->
            FilterState config { im | resultRefs_ = Dict.remove (config.ref doc) resultRefs }

          _ ->
            priorShell)
    |> Maybe.withDefault priorShell
