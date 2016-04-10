module Pylon.DB.Legacy.Group
  ( bindingGroupTo
  , dataRebasedBinding
  , dataSubBinding
  , forwardingGroupTo
  , groupDataSubscriber
  , groupRebasedBinding
  , groupRebasedDataSubscriber
  , groupSubBinding
  , groupSubscriber
  , orderGroupBy
  , sendingGroupTo 
  ) where

{-| This is being kept in a seperate legacy module where it will not clutter things. Since a lot
of code is based on it for now, it is being kept intact. Do not use this module if you are not already
heavily invested in an existing project that uses these.

@docs bindingGroupTo, dataRebasedBinding, dataSubBinding, forwardingGroupTo, groupDataSubscriber, groupRebasedBinding, groupRebasedDataSubscriber, groupSubBinding, groupSubscriber, orderGroupBy, sendingGroupTo
-}

import Pylon.App as App
import Pylon.Resource as Resource exposing (Resource)
import Pylon.DB as DB
import Pylon.DB.Group exposing (..)

import ElmFire

import Task exposing (Task, andThen, onError)
import Dict exposing (Dict)
import Set exposing (Set)


groupDrain__ : Signal.Mailbox (List (GroupFeedback subfeedback))
groupDrain__ = Signal.mailbox []


{-| DEPRECIATED, removed in 6.0.0

Nested group subscription binding. -}
type alias GroupBinding subfeedback =
  { location : ElmFire.Location
  , address : Signal.Address (List (GroupFeedback subfeedback))
  , ordering : ElmFire.OrderOptions
  }


{-| DEPRECIATED, removed in 6.0.0

Construct a new incomplete group binding from a ElmFire Firebase location. -}
bindingGroupTo : ElmFire.Location -> GroupBinding subfeedback
bindingGroupTo location =
  { location = location
  , address = groupDrain__.address
  , ordering = ElmFire.noOrder
  }


{-| DEPRECIATED, removed in 6.0.0

Give an ordering for the group subscription's internal query. Note that this _will not order the
resulting key-value dictionary_, but it is still useful because it will have an effect on which
results are produced by the Firebase API in the event that limiting is used, which is the case for
most practical applications. -}
orderGroupBy : ElmFire.OrderOptions -> GroupBinding subfeedback -> GroupBinding subfeedback
orderGroupBy ordering groupBinding =
  { groupBinding
  | ordering = ordering
  }

{-| DEPRECIATED, removed in 6.0.0

Provide an address for group feedback actions. If this is not applied to the binding, no feedback
will be recieved. -}
sendingGroupTo : Signal.Address (List (GroupFeedback subfeedback)) -> GroupBinding subfeedback -> GroupBinding subfeedback
sendingGroupTo address groupBinding =
  { groupBinding
  | address = address
  }

{-| DEPRECIATED, removed in 6.0.0

Provide an address for group feedback actions with a transformation function for forwarding. If
this is not applied to the binding, no feedback will be recieved. -}
forwardingGroupTo : (List (GroupFeedback subfeedback) -> List action) -> Signal.Address (List action) -> GroupBinding subfeedback -> GroupBinding subfeedback
forwardingGroupTo factions address =
  sendingGroupTo (Signal.forwardTo address factions)


{-| DEPRECIATED, removed in 6.0.0

A convenience function which makes it a little bit easier to declare a sub-binding function for
concrete data. Note that if you are using the `groupDataSubscriber` convenience function, then this
is already invoked internally, making the task of bindin a group of concrete records even easier. -}
dataSubBinding : DB.Config v -> GroupBinding (DB.Feedback v) -> String -> DB.Binding v
dataSubBinding config groupBinding key =
  ElmFire.sub key groupBinding.location
  |> DB.bindingFrom config
  |> DB.forwardingTo (List.map <| GroupSub key) groupBinding.address


{-| DEPRECIATED, removed in 6.0.0

A convenience function which makes it a little bit easier to declare a sub-binding function for
nested groups. -}
groupSubBinding : ElmFire.OrderOptions -> GroupBinding (GroupFeedback subfeedback') -> String -> GroupBinding subfeedback'
groupSubBinding ordering groupBinding key =
  ElmFire.sub key groupBinding.location
  |> bindingGroupTo
  |> orderGroupBy ordering
  |> forwardingGroupTo (List.map <| GroupSub key) groupBinding.address


{-| DEPRECIATED, removed in 6.0.0

Convenience function that makes it easier to bind a `Group (DB.Data v)`. -}
groupDataSubscriber : DB.Config v -> GroupBinding (DB.Feedback v) -> Group (DB.Data v) -> (Group (DB.Data v), List (DB.DBTask never))
groupDataSubscriber config =
  groupSubscriber DB.cancel DB.subscribe (dataSubBinding config)



{-| DEPRECIATED, removed in 6.0.0

Rebased binding uses the key of the discovered item to get a child at a diffeent location than
that of the group binding. A good example would be a subscription that tracks a group of users,
but actually pulls data from a global profiles list. -}
dataRebasedBinding : ElmFire.Location -> DB.Config v -> GroupBinding (DB.Feedback v) -> String -> DB.Binding v
dataRebasedBinding base config groupBinding key =
  ElmFire.sub key base
  |> DB.bindingFrom config
  |> DB.forwardingTo (List.map <| GroupSub key) groupBinding.address


{-| DEPRECIATED, removed in 6.0.0

Rebased binding for nested groups. -}
groupRebasedBinding : ElmFire.Location -> ElmFire.OrderOptions -> GroupBinding (GroupFeedback subfeedback') -> String -> GroupBinding subfeedback'
groupRebasedBinding base ordering groupBinding key =
  ElmFire.sub key base
  |> bindingGroupTo
  |> orderGroupBy ordering
  |> forwardingGroupTo (List.map <| GroupSub key) groupBinding.address


{-| DEPRECIATED, removed in 6.0.0

Convenience function that makes it easier to bind a `Group (DB.Data v)`. -}
groupRebasedDataSubscriber : ElmFire.Location -> DB.Config v -> GroupBinding (DB.Feedback v) -> Group (DB.Data v) -> (Group (DB.Data v), List (DB.DBTask never))
groupRebasedDataSubscriber base config =
  groupSubscriber DB.cancel DB.subscribe (dataRebasedBinding base config)


{-| DEPRECIATED, removed in 6.0.0

This is the old subscription primitive, and is quite a bit more limited. For testing purposes,
it has been implemented in terms of `groupIntegrate` and `groupSubscription`. `groupIntegrate` is
much more powerful, and takes a list of "sourcing" functions like `groupSubscription`. -}
groupSubscriber
  :  (subtype -> (subtype, List (DB.DBTask never)))
  -> (subbinding -> subtype -> (subtype, List (DB.DBTask never)))
  -> (GroupBinding subfeedback -> String -> subbinding)
  -> GroupBinding subfeedback -> Group subtype -> (Group subtype, List (DB.DBTask never))
groupSubscriber cancelSub integrateSub bindSub' binding priorGroup =
  let
    groupConfig =
      { binding = (\_ -> bindSub' binding)
      , address = binding.address
      }

  in
    groupIntegrate
      [ groupSubscription binding.location binding.ordering
      ] cancelSub integrateSub groupConfig priorGroup
