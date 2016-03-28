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


module Pylon.DB.Meta
  ( MetaFeedback

  , MetaBinding
  , MetaData

  , nilMetaData
  , metaBindingTo
  , metaSendingTo
  , metaForwardingTo
  , metaInputOne
  , metaInput

  , getTimestampOffset
  , getConnectionStatus
  , getAuthenticationStatus

  , metaSubscribe

  , serverTimestampPlaceholder
  ) where

{-| Subscribe to metadata about connectivity, timestamp offset, and authentication using the
same easy to understand pattern set forth in `Pylon.DB`.

# Types
@docs MetaFeedback, MetaBinding, MetaData

# Special Server Metadata Binding
@docs nilMetaData, metaBindingTo, metaSendingTo, metaForwardingTo, metaInputOne, metaInput, metaSubscribe

# Getters
@docs getTimestampOffset, getConnectionStatus, getAuthenticationStatus

# Etc.
@docs serverTimestampPlaceholder

-}


import Pylon.App as App
import Pylon.Resource as Resource exposing (Resource)
import Pylon.DB as DB

import ElmFire
import ElmFire.Auth

import Json.Encode

import Task exposing (Task, andThen, onError)
import Time exposing (Time)


--/--------------------------------------\--
--| SPECIAL SERVER METADATA SUBSCRIPTION |--
--\--------------------------------------/--


metaDrain_ : Signal.Mailbox (List MetaFeedback)
metaDrain_ = Signal.mailbox []


{-| Feedback for the the server metadata subscription. -}
type MetaFeedback =
  TimeStampOffset Time

  | AuthStatus (Maybe ElmFire.Auth.Authentication)

  | Connected
  | Disconnected

  | SubscribedAuth
  | SubscribedTimeStampOffset ElmFire.Subscription
  | SubscribedConnectivity ElmFire.Subscription

  | AuthSubscriptionError ElmFire.Error
  | TimeStampOffsetSubscriptionError ElmFire.Error
  | ConnectivitySubscriptionError ElmFire.Error

  | RetryAuth
  | RetryTimeStampOffset
  | RetryConnectivity


type alias MetaSubscriptions =
  { connectivity : Resource DB.DBError ElmFire.Subscription
  , offsets : Resource DB.DBError ElmFire.Subscription
  , auth : Resource DB.DBError ()
  }


{-| Server metadata binding. -}
type alias MetaBinding =
  { address : Signal.Address (List MetaFeedback)
  , location : ElmFire.Location
  }


{-| Server metadata record. -}
type alias MetaData =
  { timeStampOffset : Resource DB.DBError Time
  , isConnected : Resource DB.DBError Bool
  , authentication : Resource DB.DBError ElmFire.Auth.Authentication
  , subscriptions : MetaSubscriptions
  }


nilMetaSubscriptions : MetaSubscriptions
nilMetaSubscriptions =
  { connectivity = Resource.unknown
  , offsets = Resource.unknown
  , auth = Resource.unknown
  }


{-| Empty metadata record. A metadata record should always be initialized to this. -}
nilMetaData : MetaData
nilMetaData =
  { timeStampOffset = Resource.unknown
  , isConnected = Resource.unknown
  , authentication = Resource.unknown
  , subscriptions = nilMetaSubscriptions
  }


{-| Get the resource representing the current timestamp offset to the server. -}
getTimestampOffset : MetaData -> Resource DB.DBError Time
getTimestampOffset = .timeStampOffset


{-| Get the resource representing the current connection status to the server. -}
getConnectionStatus : MetaData -> Resource DB.DBError Bool
getConnectionStatus = .isConnected


{-| Get the resource representing the current authentication status to the server. -}
getAuthenticationStatus : MetaData -> Resource DB.DBError ElmFire.Auth.Authentication
getAuthenticationStatus = .authentication


{-| Create a metadata binding for a firebase location. Should be the root location. -}
metaBindingTo : ElmFire.Location -> MetaBinding
metaBindingTo location =
  { location = location
  , address = metaDrain_.address
  }


{-| Add an address to a metadata binding. Unless you do this, feedback will go to a hidden internal
drain. -}
metaSendingTo : Signal.Address (List MetaFeedback) -> MetaBinding -> MetaBinding
metaSendingTo address binding =
  { binding
  | address = address
  }


{-| Add an address to a metadata binding with a forwarding function. -}
metaForwardingTo : (List MetaFeedback -> List action) -> Signal.Address (List action) -> MetaBinding -> MetaBinding
metaForwardingTo factions address =
  metaSendingTo (Signal.forwardTo address factions)


{-| Server metadata single update function. -}
metaInputOne : MetaFeedback -> MetaData -> MetaData
metaInputOne feedback data =
  data.subscriptions
  |> \priorSubscriptions -> case feedback of
      TimeStampOffset toffset ->
        { data | timeStampOffset = Resource.def toffset }

      AuthStatus maybeAuth ->
        { data
        | authentication =
            Maybe.map Resource.def maybeAuth
            |> Maybe.withDefault Resource.void
        }

      Connected ->
        { data | isConnected = Resource.def True }

      Disconnected ->
        { data | isConnected = Resource.def False }

      SubscribedTimeStampOffset subscription ->
        { data
        | subscriptions =
            { priorSubscriptions
            | offsets = Resource.def subscription
            }
        , timeStampOffset = Resource.deriveIf Resource.isNotKnown (always Resource.pending) data.timeStampOffset
        }

      SubscribedAuth ->
        { data
        | subscriptions =
            { priorSubscriptions
            | auth = Resource.def ()
            }
        , authentication = Resource.deriveIf Resource.isNotKnown (always Resource.pending) data.authentication
        }

      SubscribedConnectivity subscription ->
        { data
        | subscriptions =
            { priorSubscriptions
            | connectivity = Resource.def subscription
            }
        , isConnected = Resource.deriveIf Resource.isNotKnown (always Resource.pending) data.isConnected
        }


      TimeStampOffsetSubscriptionError error ->
        { data
        | subscriptions =
            { priorSubscriptions
            | offsets = Resource.undecided (DB.SubscriptionErrorTag error)
            }
        , timeStampOffset = Resource.pending
        }

      AuthSubscriptionError error ->
        { data
        | subscriptions =
            { priorSubscriptions
            | auth = Resource.undecided (DB.SubscriptionErrorTag error)
            }
        , authentication = Resource.pending
        }

      ConnectivitySubscriptionError error ->
        { data
        | subscriptions =
            { priorSubscriptions
            | connectivity = Resource.undecided (DB.SubscriptionErrorTag error)
            }
        , isConnected = Resource.pending
        }

      RetryAuth ->
        { data
        | subscriptions =
            { priorSubscriptions
            | auth = Resource.unknown
            }
        , authentication = Resource.unknown
        }

      RetryTimeStampOffset ->
        { data
        | subscriptions =
            { priorSubscriptions
            | offsets = Resource.unknown
            }
        , timeStampOffset = Resource.unknown
        }

      RetryConnectivity ->
        { data
        | subscriptions =
            { priorSubscriptions
            | connectivity = Resource.unknown
            }
        , isConnected = Resource.unknown
        }


{-| Server metadata list update function. -}
metaInput : List MetaFeedback -> MetaData -> MetaData
metaInput feedbacks data =
  case feedbacks of
    [] -> data
    y :: [] -> metaInputOne y data
    x :: xs' -> metaInputOne x data |> metaInput xs'


metaSubscribeAuth : MetaBinding -> MetaData -> (MetaData, List (DB.DBTask never))
metaSubscribeAuth binding data =
  let
    address =
      binding.address


    priorSubscriptions =
      data.subscriptions


    retryTasks =
      (Task.sleep (8.0 * Time.second)
        `andThen` \_ -> Signal.send binding.address [RetryAuth])
      |> App.finalizeTask

    markedAsPending tasks' =
      (,)
        { data
        | subscriptions =
            { priorSubscriptions
            | auth = Resource.pending
            }
        , authentication = Resource.pending
        } tasks'

  in
    priorSubscriptions.auth
    |> Resource.therefore (always (data, [ ])) -- do nothing to a live subscription
    |> Resource.deriveIf Resource.isUnknown
        (\_ -> ElmFire.Auth.subscribeAuth --(Maybe.Maybe ElmFire.Auth.Authentication -> Task.Task x a) (ElmFire.Location -> Task.Task ElmFire.Error )
            (\maybeAuth -> [AuthStatus maybeAuth]
            |> Signal.send address)
            binding.location
        |> Task.toResult
        |> Task.map (\result' -> case result' of
            Result.Ok () -> Signal.send address [SubscribedAuth]
            Result.Err error -> Signal.send address [AuthSubscriptionError error])
        |> App.finalizeTask
        |> markedAsPending
        |> Resource.def)
    |> Resource.decideBy
        (\_ -> markedAsPending retryTasks
        |> Resource.def)
    |> Resource.otherwise (data, [ ])



metaSubscribeConnectivity : MetaBinding -> MetaData -> (MetaData, List (DB.DBTask never))
metaSubscribeConnectivity binding data =
  let
    address =
      binding.address


    priorSubscriptions =
      data.subscriptions


    retryTasks =
      (Task.sleep (8.0 * Time.second)
        `andThen` \_ -> Signal.send binding.address [RetryConnectivity])
      |> App.finalizeTask

    markedAsPending tasks' =
      (,)
        { data
        | subscriptions =
            { priorSubscriptions
            | connectivity = Resource.pending
            }
        , isConnected = Resource.pending
        } tasks'

  in
    priorSubscriptions.connectivity
    |> Resource.therefore (always (data, [ ])) -- do nothing to a live subscription
    |> Resource.deriveIf Resource.isUnknown
        (\_ -> ElmFire.subscribeConnected
            (\isConnected -> (if isConnected then [Connected] else [Disconnected])
            |> Signal.send address)
            binding.location
        |> Task.toResult
        |> Task.map (\result' -> case result' of
            Result.Ok subscription -> Signal.send address [SubscribedConnectivity subscription]
            Result.Err error -> Signal.send address [ConnectivitySubscriptionError error])
        |> App.finalizeTask
        |> markedAsPending
        |> Resource.def)
    |> Resource.decideBy
        (\_ -> markedAsPending retryTasks
        |> Resource.def)
    |> Resource.otherwise (data, [ ])


metaSubscribeTimeStampOffset : MetaBinding -> MetaData -> (MetaData, List (DB.DBTask never))
metaSubscribeTimeStampOffset binding data =
  let
    address =
      binding.address


    priorSubscriptions =
      data.subscriptions


    retryTasks =
      (Task.sleep (8.0 * Time.second)
        `andThen` \_ -> Signal.send binding.address [RetryTimeStampOffset])
      |> App.finalizeTask

    markedAsPending tasks' =
      (,)
        { data
        | subscriptions =
            { priorSubscriptions
            | offsets = Resource.pending
            }
        , timeStampOffset = Resource.pending
        } tasks'

  in
    priorSubscriptions.offsets
    |> Resource.therefore (always (data, [ ])) -- do nothing to a live subscription
    |> Resource.deriveIf Resource.isUnknown
        (\_ -> ElmFire.subscribeServerTimeOffset
            (\toffset -> Signal.send address [TimeStampOffset toffset])
            binding.location
        |> Task.toResult
        |> Task.map (\result' -> case result' of
            Result.Ok subscription -> Signal.send address [SubscribedTimeStampOffset subscription]
            Result.Err error -> Signal.send address [TimeStampOffsetSubscriptionError error])
        |> App.finalizeTask
        |> markedAsPending
        |> Resource.def)
    |> Resource.decideBy
        (\_ -> markedAsPending retryTasks
        |> Resource.def)
    |> Resource.otherwise (data, [ ])


{-| Subscribe to the server metadata at your root Firebase location. -}
metaSubscribe : MetaBinding -> MetaData -> (MetaData, List (DB.DBTask never))
metaSubscribe binding data =
  App.chain
    [ metaSubscribeAuth binding
    , metaSubscribeConnectivity binding
    , metaSubscribeTimeStampOffset binding
    ] data



{-| A placeholder for encoding the server timestamp. -}
serverTimestampPlaceholder : Json.Encode.Value
serverTimestampPlaceholder = ElmFire.serverTimeStamp
