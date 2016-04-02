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


module Pylon.DB
  ( DBTask, DBError(..), Feedback(..)
  , Config, Binding, Operation
  , Data

  , opSet
  , opSetAndPrioritize
  , opPrioritize
  , opUpdateChildren
  , opSetOrUpdateChildren
  , opDelete

  , config
  , configDecoder
  , configEncoder
  , nilConfig
  , withEncoder
  , withDecoder
  , bindingTo
  , bindingFrom
  , sendingTo
  , forwardingTo

  , newData
  , voidData

  , subscribe
  , cancel
  , reset
  , cancelAndReset
  , inputOne
  , input

  , getResource
  , getPriorResource
  , getLastFailed

  , doOperationIf
  , doMap
  , doTransform
  , enqueueOperation
  , flushQueue
  , doOperation
  , doRetry
  , doOperationSimple
  ) where

{-| High level 2-way data binding against Firebase using ElmFire. For 2-way binding of arbitrary,
possibly nested collections of data, use `Pylon.DB.Group`. You should have ElmFire installed to
use this, and in order to avoid redundancy. A more general API that can be wired to other DB
backends is planned, but not promised.

# Types
@docs DBTask, DBError, Feedback, Config, Binding, Operation, Data

# Define Operations
@docs opSet, opSetAndPrioritize, opPrioritize, opUpdateChildren, opSetOrUpdateChildren, opDelete

# Configuration
@docs config, configDecoder, configEncoder, nilConfig, withEncoder, withDecoder, bindingTo, bindingFrom, sendingTo, forwardingTo

# Data Constructors
@docs newData, voidData

# Data Binding
@docs subscribe, cancel, reset, cancelAndReset, inputOne, input

# Interrogating Data
@docs getResource, getPriorResource, getLastFailed

# Operate on Data
@docs doOperationIf, doMap, doTransform, enqueueOperation, flushQueue, doOperation, doRetry, doOperationSimple

-}

import Pylon.App as App
import Pylon.Resource as Resource exposing (Resource)

import ElmFire

import Json.Encode
import Json.Decode

import Task exposing (Task, andThen, onError)
import Dict exposing (Dict)


{-| An alias for `App.FinalTask`. Kept for legacy reasons in our proprietary codebase. -}
type alias DBTask never = App.FinalTask never


{-| A database error. -}
type DBError =
  QueryErrorTag ElmFire.Error
  | PermissionErrorTag String
  | DataUnavailableErrorTag String
  | OperationErrorTag ElmFire.Error
  | SubscriptionErrorTag ElmFire.Error
  | DecodingErrorTag String


{-| Feedback from the database. -}
type Feedback v =
  Subscribed ElmFire.Subscription
  | SubscriptionError ElmFire.Error
  | Cancelled
  | DecodingFailed String
  | QueryError ElmFire.Error
  | Updated v
  | UpdatedToVoid
  | OperationError (Operation v) ElmFire.Error


{-| Data configuration, consisting of a JSON decoder/encoder pair. -}
type alias Config v =
  { decoder : Json.Decode.Decoder v
  , encoder : v -> Json.Encode.Value
  }


{-| Data binding. Consists of a `Config`, an address for `Feedback`, and a location in Firebase. -}
type alias Binding v =
  { config : Config v
  , address : Signal.Address (List (Feedback v))
  , location : ElmFire.Location
  }


{-| Opaque type for a database operation. -}
type Operation v =
  Set v
  | SetAndPrioritize v ElmFire.Priority
  | Prioritize ElmFire.Priority
  | UpdateChildren v
  | SetOrUpdateChildren v
  | Delete


{-| Represents a piece of data that may be bound to a Firebase location. -}
type alias Data v =
  { subscription : Resource DBError ElmFire.Subscription
  , value : Resource DBError v
  , priorValue : Resource DBError v
  , lastFailed : Maybe (Operation v)
  , queue : List (Operation v)
  }


{-| Set the data to some value. -}
opSet : v -> Operation v
opSet = Set

{-| Set the data to some value and give it a Firebase priority. -}
opSetAndPrioritize : v -> ElmFire.Priority -> Operation v
opSetAndPrioritize = SetAndPrioritize

{-| Give the data a Firebase priority. -}
opPrioritize : ElmFire.Priority -> Operation v
opPrioritize = Prioritize

{-| Update the children in the JSON structure of the data. This diffs the value of the data
currently against the given value after encoding so that a minimal update is always done.

TODO : This may not work right in the event that the data has been updated remotely but not yet
synced to the server. One way around this might be to manually fetch the data first as part of
the operation. -}
opUpdateChildren : v -> Operation v
opUpdateChildren = UpdateChildren

{-| Combination operation that sets the data if the data is known to be void, or else performs the
above listed `opUpdateChildren`. -}
opSetOrUpdateChildren : v -> Operation v
opSetOrUpdateChildren = SetOrUpdateChildren

{-| Delete the bound data. -}
opDelete : Operation v
opDelete = Delete


{-| An empty, unbound data. Always initialize instance of Data to this and then invoke `subscribe`
to enable data flow back to the client. -}
newData : Data v
newData =
  { subscription = Resource.unknown
  , value = Resource.unknown
  , priorValue = Resource.void
  , lastFailed = Nothing
  , queue = []
  }


{-| A new void data item, which will not be bound by a subscription until it is `reset`. -}
voidData : Data v
voidData =
  { subscription = Resource.void
  , value = Resource.void
  , priorValue = Resource.void
  , lastFailed = Nothing
  , queue = []
  }


{-| Declare a data configuration with a decoder/encoder pair. -}
config : Json.Decode.Decoder v -> (v -> Json.Encode.Value) -> Config v
config decoder encoder =
  { decoder = decoder
  , encoder = encoder
  }


{-| Declare a data configuration with a decoder, but a dummy encoder. This is useful when you only
want one way binding from the server, or if you wish to declare the encoder and encoder seperately
for aesthetic reasons. -}
configDecoder : (Json.Decode.Decoder v) -> Config v
configDecoder decoder =
  { nilConfig
  | decoder = decoder
  }


{-| Same as above, but declare only an encoder. Offered for symmetry. -}
configEncoder : (v -> Json.Encode.Value) -> Config v
configEncoder encoder =
  { nilConfig
  | encoder = encoder
  }


{-| A configuration that hasn't yet been filled out. The decoder always fails, and the encoder
always encodes a `null`. Used by `configDecoder` and `configEncoder`. -}
nilConfig : Config v
nilConfig =
  { decoder = Json.Decode.fail "No configuration set for DB decoder."
  , encoder = always Json.Encode.null
  }


{-| Add an encoder to an existing configuration. -}
withEncoder : (v -> Json.Encode.Value) -> Config v -> Config v
withEncoder encoder config =
  { config
  | encoder = encoder
  }


{-| Add a decoder to an existing configuration. -}
withDecoder : Json.Decode.Decoder v -> Config v -> Config v
withDecoder decoder config =
  { config
  | decoder = decoder
  }


cancellationSubscription__ : ElmFire.Cancellation -> ElmFire.Subscription
cancellationSubscription__ cancellation =
  case cancellation of
    ElmFire.Unsubscribed subscription -> subscription
    ElmFire.QueryError subscription _ -> subscription


drain__ : Signal.Mailbox (List a)
drain__ = Signal.mailbox []


{-| Create a new data binding from a configuration and an ElmFire location. -}
bindingTo : ElmFire.Location -> Config v -> Binding v
bindingTo location config =
  { config = config
  , address = drain__.address
  , location = location
  }


{-| Flipped version of `bindingTo` that composes more naturally in the event that your configuration
was created elsewhere and reused with a new ElmFire location. -}
bindingFrom : Config v -> ElmFire.Location -> Binding v
bindingFrom = flip bindingTo


{-| Set the `Feedback` address for a data binding. By default, data bindings are bound to a
hidden drain so that you can more comfortably construct bindings without being sure of the target
address right away. -}
sendingTo : Signal.Address (List (Feedback v)) -> Binding v -> Binding v
sendingTo address binding =
  { binding
  | address = address
  }


{-| Set the `Feedback` address for a data binding to some forwarding address. Commonly, this is used
to nest `Feedback` within an action type from your application. -}
forwardingTo : (List (Feedback v) -> List action) -> Signal.Address (List action) -> Binding v -> Binding v
forwardingTo faction address =
  sendingTo (Signal.forwardTo address faction)


{-| `subscribe` is an effector (see Pylon.App for what what we define an effector to be) that takes
a `Binding` and a `Data`. The resulting tasks when executed will set up 2 way data binding using the
`Config` within the `Binding` to encode and decode the data, and sending all feedback to the
address within the `Binding`.  _Take care to set an address on your `Binding`_, or else the
feedback from the database _will not be recieved by your application._ -}
subscribe : Binding v -> Data v -> (Data v, List (DBTask never))
subscribe binding data =
  let
    (address, config, location) =
      (binding.address, binding.config, binding.location)

    maybeSubscription =
      Resource.maybeKnown data.subscription

    updateFromSnapshot snapshot =
      if snapshot.existing then
        ElmFire.exportValue snapshot                    -- export the snapshot to a json object
        |> Json.Decode.decodeValue config.decoder       -- decode that json object using the user supplied decoder
        |> Result.map (\data' -> [Updated data'])                  -- map successful decoding to an update
        |> Result.formatError (\error -> [DecodingFailed error]) -- map format error to decoding failure report
        |> \result' -> case result' of
          Result.Ok actions -> Signal.send address actions
          Result.Err actions -> Signal.send address actions
      else
        Signal.send address [UpdatedToVoid]


    updateFromCancellation cancellation =
      if Just (cancellationSubscription__ cancellation) == maybeSubscription then
        case cancellation of
          ElmFire.Unsubscribed _ -> Signal.send address [Cancelled]
          ElmFire.QueryError _ error -> Signal.send address [QueryError error]

      else
        Task.succeed ()


    subscriptionTasks =
      if Resource.isUnknown data.subscription then
        (ElmFire.subscribe
          updateFromSnapshot updateFromCancellation
          (ElmFire.valueChanged ElmFire.noOrder) location
        `andThen` (\subscription -> Signal.send address [Subscribed subscription])
        `onError` (\error -> Signal.send address [SubscriptionError error]))
        |> \task' -> [task']
      else
        [ ]

    subscription' =
      Resource.deriveIf Resource.isUnknown (\_ -> Resource.pending) data.subscription

    value' =
      if Resource.isUnknown data.subscription then
        Resource.pending
      else
        data.value
  in
    ( { data
      | subscription = subscription'
      , value = value'
      , priorValue = data.value
      }
    , subscriptionTasks
      |> App.finalizeTasks App.parallel
    )


{-| Cancel your subscription to the data. This puts the data in a void state, so that the
`subscribe` function ceases to have any effect. In order to reactivate the binding, you must
call `reset`. A convenience function `cancelAndReset` is provided, which allows you to skip the
second call if you simply wish to refresh, but not disable, the data binding. -}
cancel : Data v -> (Data v, List (DBTask never))
cancel data =
  Resource.maybeKnown data.subscription
  |> Maybe.map (\subs ->
    [ ElmFire.unsubscribe subs
        `andThen` (\_ -> Task.succeed ())
        `onError` (\_ -> Task.succeed ())
    ])
  |> Maybe.withDefault [ ]
  |> App.finalizeTasks App.parallel
  |> (,) { data | subscription = Resource.void }


{-| Reset the given data from a void or error state so that the binding can be attempted again the
next time `subscribe` is reached. Notice that this is a simple transformation and not an effector,
so if the data binding is live, this will have no effect. If you wish to cancel an existing
subscription that is live and has not erred, then you should use `cancel`, or `cancelAndReset` in
the case that you wish to rebind the data right away. -}
reset : Data v -> Data v
reset data =
  { data
  | subscription =
      Resource.deriveIf Resource.isVoid (always Resource.unknown) data.subscription
      |> Resource.deriveIf Resource.isUndecided (always Resource.unknown)
  }

{-| A convenience function that cancels any current bindings that might be live, then `reset`s the
structure so that the next time `subscribe` is reached the bindings will be reactivated immediately.
-}
cancelAndReset : Data v -> (Data v, List (DBTask never))
cancelAndReset data =
  cancel data
  |> \(data', tasks) -> reset data'
  |> flip (,) tasks


{-| Single input update function for `Data`. -}
inputOne : Feedback v -> Data v -> Data v
inputOne feedback data =
  case feedback of
    Subscribed subs ->
      { data
      | subscription = Resource.def subs
      }
    SubscriptionError error ->
      { data
      | subscription = Resource.undecided (SubscriptionErrorTag error)
      }
    Cancelled ->
      { data
      | value =
          if Resource.isPending data.subscription
          || Resource.isKnown data.subscription
          || Resource.isUndecided data.subscription then
            Resource.void
          else
            data.value
      , priorValue = data.value
      }

    DecodingFailed error ->
      { data
      | value = Resource.undecided (DecodingErrorTag error)
      , priorValue = data.value
      }

    QueryError error ->
      { data
      | subscription = Resource.undecided (QueryErrorTag error)
      }

    Updated value' ->
      { data
      | value = Resource.def value'
      , priorValue = data.value
      }

    UpdatedToVoid ->
      { data
      | value = Resource.void
      , priorValue = data.value
      }

    OperationError op error ->
      { data
      | subscription = Resource.undecided (OperationErrorTag error)
      , lastFailed = Just op
      }


{-| Input list update function for `Data`. -}
input : List (Feedback v) -> Data v -> Data v
input feedbacks data =
  case feedbacks of
    [] -> data
    y :: [] -> inputOne y data
    x :: xs' -> inputOne x data |> input xs'


{-| Get the underlying resource tracked in the `Data` record. -}
getResource : Data v -> Resource DBError v
getResource = .value


{-| Get the prior value of the resource tracked in the `Data` record, as of the last time it changed. -}
getPriorResource : Data v -> Resource DBError v
getPriorResource = .priorValue


{-| Get the last operation that failed on the record, if it exists. -}
getLastFailed : Data v -> Maybe (Operation v)
getLastFailed = .lastFailed


{-| Conditionally dispatch an operation on a bound `Data` record. -}
doOperationIf : (Resource DBError v -> Bool) -> Binding v -> Operation v -> Data v -> (Data v, List (DBTask never))
doOperationIf predicate binding op data =
  if predicate data.value then
    doOperation binding op data
  else
    (data, [])


{-| Map the current value of a bound `Data` record _iff_ the value is currently known. -}
doMap : Binding v -> (v -> v) -> Data v -> (Data v, List (DBTask never))
doMap binding xdcr data =
  Resource.maybeKnown data.value
  |> Maybe.map
    (\value -> xdcr value
    |> \value' -> doOperation binding (opSet value') data)
  |> Maybe.withDefault (data, [])


{-| Transform the resource within a bound `Data` record. Do not use this unless you absolutely know what you are doing! -}
doTransform : Binding v -> (Resource DBError v -> Resource DBError v) -> Data v -> (Data v, List (DBTask never))
doTransform binding reduction data =
  reduction data.value
  |> \res' -> Resource.maybeKnown res'
  |> Maybe.map (\value -> doOperation binding (opSet value) data)
  |> Maybe.withDefault
    (if Resource.isVoid res' then
      doOperation binding opDelete data
    else
      (,) { data | value = res', priorValue = data.value } [])


{-| Add an operation to the queue. -}
enqueueOperation : Operation v -> Data v -> Data v
enqueueOperation op data =
  { data | queue = op :: data.queue }


flushQueue__ : Binding v -> Data v -> (Data v, List (DBTask never))
flushQueue__ binding data =
  case data.queue of
    [] -> (data, [])
    [op] -> doOperation binding op data
    op :: queue' ->
      flushQueue binding { data | queue = queue' }
      |> \(data', tasks') -> doOperation binding op data'
      |> \(data_, tasks_) -> (data_, tasks' ++ tasks_)

{-| Flush the queue of pending operations, resulting in a list of tasks. -}
flushQueue : Binding v -> Data v -> (Data v, List (DBTask never))
flushQueue binding data =
  flushQueue__ binding data
  |> \(data', tasks') -> (,) { data | queue = [] } tasks'


valueToPairs_ : Config v -> v -> Maybe (Dict String Json.Encode.Value)
valueToPairs_ config value =
  config.encoder value
  |> Json.Decode.decodeValue (Json.Decode.dict Json.Decode.value)
  |> Result.toMaybe


updatedChildren_' : Config v -> v -> v -> Json.Encode.Value
updatedChildren_' config curr next =
  let
    doDiff currPairs nextPairs =
      Dict.foldl (\key value ls ->
        Dict.get key currPairs
        |> Maybe.map (\currValue -> if currValue == value then ls else (key, value) :: ls)
        |> Maybe.withDefault ((key, value) :: ls)
      ) [] nextPairs
      |> Json.Encode.object


  in
    Maybe.map2 doDiff (valueToPairs_ config curr) (valueToPairs_ config next)
    |> Maybe.withDefault (config.encoder next)


updatedChildren_ : Config v -> Resource DBError v -> v -> (Bool, Json.Encode.Value)
updatedChildren_ config curr next =
  Resource.therefore (flip (updatedChildren_' config) next >> (,) True) curr
  |> Resource.otherwise (False, config.encoder next)


{-| Dispatch the given operation without binding to data. -}
doOperationSimple : ElmFire.Location -> Config v -> Operation v -> List (DBTask never)
doOperationSimple loc config op =
  case op of
    Set value' ->
      ElmFire.set (config.encoder value') loc
      |> App.finalizeTask

    SetAndPrioritize value' priority ->
      ElmFire.setWithPriority (config.encoder value') priority loc
      |> App.finalizeTask

    Prioritize priority ->
      ElmFire.setPriority priority loc
      |> App.finalizeTask

    UpdateChildren value' ->
      Debug.crash "UpdateChildren is not implemented for doOperationSimple"

    SetOrUpdateChildren value' ->
      Debug.crash "SetOrUpdateChildren is not implemented for doOperationSimple"

    Delete ->
      ElmFire.remove loc
      |> App.finalizeTask


{-| Dispatch the given operation immediately on a bound `Data` record. -}
doOperation : Binding v -> Operation v -> Data v -> (Data v, List (DBTask never))
doOperation binding op data =
  let
    processResult data =
      Task.map (\ref -> Task.succeed ())
      >> Task.mapError (\error -> Signal.send binding.address [OperationError op error])
      >> App.finalizeTask
      >> (,) { data | value = Resource.pending, priorValue = data.value }

  in
    case op of
      Set value' ->
        ElmFire.set (binding.config.encoder value') binding.location
        |> processResult data

      SetAndPrioritize value' priority ->
        ElmFire.setWithPriority (binding.config.encoder value') priority binding.location
        |> processResult data

      Prioritize priority ->
        ElmFire.setPriority priority binding.location
        |> processResult data

      UpdateChildren value' ->
        ElmFire.update (snd <| updatedChildren_ binding.config data.value value') binding.location
        |> processResult data

      SetOrUpdateChildren value' ->
        case updatedChildren_ binding.config data.value value' of
          (True, diff) ->
            ElmFire.update diff binding.location
            |> processResult data

          (False, value) ->
            ElmFire.set value binding.location
            |> processResult data

      Delete ->
        ElmFire.remove binding.location
        |> processResult data



{-| Retry the last operation that failed on a given bound `Data` record in the case that any
exists. -}
doRetry : Binding v -> Data v -> (Data v, List (DBTask never))
doRetry binding data =
  case data.lastFailed of
    Just op -> doOperation binding op { data | lastFailed = Nothing }
    Nothing -> (data, [])
