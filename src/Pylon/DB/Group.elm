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


module Pylon.DB.Group
  ( GroupFeedback(..)
  , GroupConfig
  , Group

  , getGroupCurrentData
  , getGroupDeltaData
  , getGroupDataResDeltas
  , getGroupDataResDeltaList
  , groupDataResDeltaFoldL
  , groupDataResDeltaFoldR

  , groupUpdateSub
  , groupDoSub
  , groupDoEachSub
  , groupAddSub
  , groupRemoveSub
  , groupDeriveSub
  , getGroupSubData

  , newGroup
  , voidGroup

  , groupInputOne
  , groupInput
  , groupDataInputOne
  , groupDataInput

  , resetGroup
  , cancelGroup
  , cancelDataGroup
  , cancelAndResetGroup
  , cancelAndResetDataGroup

  , groupIntegrate
  , groupSubscription
  ) where

{-| Nested data. The typing is done such that one can arbitrarily nest groups, trivial data records,
or even a compatible API fitting the same pattern such that this system is easily extensible.

# Types
@docs GroupFeedback, GroupConfig, Group

# Direct Group Inquiry
@docs getGroupCurrentData, getGroupDeltaData, getGroupDataResDeltas, getGroupDataResDeltaList, groupDataResDeltaFoldL, groupDataResDeltaFoldR, groupDeriveSub, getGroupSubData

# Direct Group Manipulation

You can use these functions to perform nested operations on groups directly. You will need this
functionality to write new items in to a group. Take care, however, that you _do not try to remove
items form the group by using `groupRemoveSub` directly._ To effect the remote data, you must
invoke the operations provided by `Pylon.DB`.

@docs groupUpdateSub, groupDoSub, groupDoEachSub, groupAddSub, groupRemoveSub

# Group Constructors
@docs newGroup, voidGroup

# Raw Group Operations
@docs groupInputOne, groupInput, cancelGroup, resetGroup, cancelAndResetGroup, groupIntegrate, groupSubscription

# Data Group Operations
@docs groupDataInputOne, groupDataInput, cancelDataGroup, cancelAndResetDataGroup

-}

import Pylon.App as App
import Pylon.Resource as Resource exposing (Resource)
import Pylon.DB as DB

import ElmFire

import Task exposing (Task, andThen, onError)
import Dict exposing (Dict)
import Set exposing (Set)


--/----------------------\--
--| NESTED ACTIVE RECORD |--
--\----------------------/--

type GroupDelta = GroupAddD | GroupRmD | GroupSubD


type alias GroupFeedbackReserved_ = ()


{-| DB.Feedback for nested group subscriptions. -}
type GroupFeedback subfeedback =
  GroupSubscribedAdd ElmFire.Subscription
  | GroupSubscribedRemove ElmFire.Subscription

  | GroupAdd String
  | GroupRemove String
  | GroupRefresh String
  | GroupSub String subfeedback

  | GroupCancelledAdd
  | GroupCancelledRemove

  | GroupAddSubscriptionError ElmFire.Error
  | GroupRemoveSubscriptionError ElmFire.Error

  | GroupReserved GroupFeedbackReserved_



{-| Specifies the binding behavior and mailbox address for a groupIntegrate -}
type alias GroupConfig subfeedback subbinding =
  { binding : Signal.Address (List (GroupFeedback subfeedback)) -> String -> subbinding
  , address : Signal.Address (List (GroupFeedback subfeedback))
  }


{-| Nested group record. -}
type alias Group subtype =
  { dataDelta : Dict String (subtype, GroupDelta)
  , data : Resource DB.DBError (Dict String subtype)

  , addSubscription : Resource DB.DBError ElmFire.Subscription
  , removeSubscription : Resource DB.DBError ElmFire.Subscription

  , currentLocation : Maybe (ElmFire.Location)
  }

-- GROUP MANIPULATION AND INQUIRY


{-| Get the current dictionary representing the group's sub-record mapping, without the most recent
deltas factored in. -}
getGroupCurrentData : Group subtype -> Dict String subtype
getGroupCurrentData =
  .data >> Resource.otherwise Dict.empty


{-| Get the current change in the group's data. -}
getGroupDeltaData : Group subtype -> Dict String (Maybe subtype)
getGroupDeltaData =
  .dataDelta
  >> Dict.map (\_ (data', deltaTag) -> if deltaTag == GroupRmD then Nothing else Just data')


{-| Get the current change in the group's data as a dictionary of resource pairs, each representing
the prior and current values of the data respectively. -}
getGroupDataResDeltas : Group (DB.Data v) -> Dict String (Resource DB.DBError v, Resource DB.DBError v)
getGroupDataResDeltas =
  groupDataResDeltaFoldL Dict.insert Dict.empty


{-| Get the current change in the group's data as a list of (key, (prior, current)) structures. -}
getGroupDataResDeltaList : Group (DB.Data v) -> List (String, (Resource DB.DBError v, Resource DB.DBError v))
getGroupDataResDeltaList =
  groupDataResDeltaFoldR (\key pair list -> (key, pair) :: list) []


{-| Fold from the left across the deltas, using a particular fold function and initial output. -}
groupDataResDeltaFoldL : (String -> (Resource DB.DBError v, Resource DB.DBError v) -> foldout -> foldout) -> foldout -> Group (DB.Data v) -> foldout
groupDataResDeltaFoldL = groupDataResDeltaFold False


{-| Fold from the right across the deltas, using a particular fold function and initial output. -}
groupDataResDeltaFoldR : (String -> (Resource DB.DBError v, Resource DB.DBError v) -> foldout -> foldout) -> foldout -> Group (DB.Data v) -> foldout
groupDataResDeltaFoldR = groupDataResDeltaFold True


groupDataResDeltaFold : Bool -> (String -> (Resource DB.DBError v, Resource DB.DBError v) -> foldout -> foldout) -> foldout -> Group (DB.Data v) -> foldout
groupDataResDeltaFold fromRight fFold foldIn group =
  let
    foldOp = if fromRight then Dict.foldr else Dict.foldl

    priorData = getGroupCurrentData group

    priorValueOf key =
      case Dict.get key priorData of
        Just prior -> prior.value
        Nothing -> Resource.void

    deltaPairOf key (data', deltaTag) foldOut =
      (case deltaTag of
        GroupRmD  -> (priorValueOf key, Resource.void)
        GroupSubD -> (priorValueOf key, data'.value)
        GroupAddD -> (priorValueOf key, data'.value))
      |> \pair -> case pair of
        (Resource.Void, Resource.Void) -> foldOut
        _ -> fFold key pair foldOut

  in
    foldOp deltaPairOf foldIn group.dataDelta


groupRemoveExistingData : String -> Group subtype -> Group subtype
groupRemoveExistingData key group =
  { group
  | data = Resource.therefore (Dict.remove key) group.data
  }

groupAlwaysInsertData : String -> subtype -> Group subtype -> Group subtype
groupAlwaysInsertData key data group =
  { group
  | data =
      Resource.therefore (Dict.insert key data) group.data
      |> Resource.otherwise (Dict.singleton key data)
      |> Resource.def
  }


{-| -}
groupDeriveSub : (subtype -> derived) -> String -> Group subtype -> Maybe derived
groupDeriveSub derived key group =
  case Dict.get key group.dataDelta of
    Just (data', GroupRmD) -> Nothing
    Just (data', deltaTag) -> Just (derived data')
    Nothing ->
      Dict.get key (getGroupCurrentData group)
      |> Maybe.map (\data -> derived data |> Just)
      |> Maybe.withDefault Nothing


{-| -}
getGroupSubData : String -> Group (DB.Data v) -> Resource DB.DBError v
getGroupSubData key group =
  groupDeriveSub .value key group
  |> Maybe.withDefault Resource.void


{-| Update the item at the given key using a transformation function. This can be used to nest the
transformations provided by `Pylon.DB`, or in the case of nested groups from this module
(`Pylon.DB.Group`), nesting can be done as many layers deep as needed. -}
groupUpdateSub : (subtype -> subtype) -> String -> Group subtype -> Group subtype
groupUpdateSub subUpdate key group =
  let
    updateAt doing data =
      Dict.insert key (subUpdate data, doing) group.dataDelta
      |> \newDelta -> { group | dataDelta = newDelta }

  in
    case Dict.get key group.dataDelta of
      Just (data', GroupRmD) -> group
      Just (data', deltaTag) -> updateAt deltaTag data'
      Nothing ->
        Dict.get key (getGroupCurrentData group)
        |> Maybe.map (updateAt GroupSubD)
        |> Maybe.withDefault group


{-| Update the data at the given key using an effector, as documented in `Pylon.App`. This can be
used to nest the effectors provided by `Pylon.DB`, or in the case of nested groups, nesting can be
done as many layers deep as needed. _NOTE_ that this is how you can perform concrete operations
`op*` on `DB.DB.Data` records bound recursively within the group. -}
groupDoSub : (subtype -> (subtype, List (DB.DBTask never))) -> String -> Group subtype -> (Group subtype, List (DB.DBTask never))
groupDoSub subDo key group =
  let
    doAt doing data =
      subDo data
      |> \(result, tasks) -> Dict.insert key (result, doing) group.dataDelta
      |> \newDelta -> ({ group | dataDelta = newDelta }, tasks)

  in
    case Dict.get key group.dataDelta of
      Just (data', GroupRmD) -> (group, [])
      Just (data', deltaTag) -> doAt deltaTag data'
      Nothing ->
        Dict.get key (getGroupCurrentData group)
        |> Maybe.map (doAt GroupSubD)
        |> Maybe.withDefault (group, [])


{-| Perform groupDoSub at every existing key after deltas. -}
groupDoEachSub : (subtype -> (subtype, List (DB.DBTask never))) -> Group subtype -> (Group subtype, List (DB.DBTask never))
groupDoEachSub subDo group =
  let
    keys = Set.union
      (group.dataDelta
      |> Dict.filter (\key (data', deltaTag) -> deltaTag /= GroupRmD)
      |> Dict.keys
      |> Set.fromList)
      (getGroupCurrentData group |> Dict.keys |> Set.fromList)

  in
    Set.foldr
      (\key (group', tasks') -> groupDoSub subDo key group'
      |> \(group_, tasks_) -> tasks_ ++ tasks'
      |> (,) group_)
      (group, []) keys


{-| Add a sub item to the group. This is how you can write new data within a group. One should use
`groupAddSub` to add a new record, then invoke a write operation to add a new item. -}
groupAddSub : subtype -> String -> Group subtype -> Group subtype
groupAddSub subNew key group =
  case Dict.get key group.dataDelta of
    Just (data', GroupRmD) -> { group | dataDelta = Dict.remove key group.dataDelta }
    Just (data', _) -> group

    Nothing ->
      Dict.get key (getGroupCurrentData group)
      |> Maybe.map (always group)
      |> Maybe.withDefault { group | dataDelta = Dict.insert key (subNew, GroupAddD) group.dataDelta }


{-| Remove a sub item from the group. Note that this should very, very rarely ever be called manually
unless the group in question is deliberately not currently bound, in which case this becomes a simple
collection type anyway. -}
groupRemoveSub : String -> Group subtype -> Group subtype
groupRemoveSub key group =
  case Dict.get key group.dataDelta of
    Just (data', GroupAddD) -> { group | dataDelta = Dict.remove key group.dataDelta }
    Just (data', GroupSubD) -> { group | dataDelta = Dict.insert key (data', GroupRmD) group.dataDelta }
    Just (data', GroupRmD) -> group

    Nothing ->
      Dict.get key (getGroupCurrentData group)
      |> Maybe.map (\data0 -> { group | dataDelta = Dict.insert key (data0, GroupRmD) group.dataDelta })
      |> Maybe.withDefault group


{-| A new group item. -}
newGroup : Group subtype
newGroup =
  { dataDelta = Dict.empty
  , data = Resource.unknown

  , addSubscription = Resource.unknown
  , removeSubscription = Resource.unknown

  , currentLocation = Nothing
  }

{-| A new void group item, which is to say that it starts in the cancelled state, so subscriptions
will have no effect when reached until a call to `resetGroup`. -}
voidGroup : Group subtype
voidGroup =
  { dataDelta = Dict.empty
  , data = Resource.void

  , addSubscription = Resource.void
  , removeSubscription = Resource.void

  , currentLocation = Nothing
  }



{-| Group feedback update function accepting one `DB.Feedback`. -}
groupInputOne : subtype -> (subfeedback -> subtype -> subtype) -> GroupFeedback subfeedback -> Group subtype -> Group subtype
groupInputOne subNew subInput feedback group =
  let
    dataOrEmpty group =
      Resource.otherwise Dict.empty group.data
      |> Resource.def

  in
    case feedback of
      GroupSubscribedAdd subscription ->
        { group
        | addSubscription = Resource.def subscription
        , data =
            group.removeSubscription
            |> Resource.therefore (always <| dataOrEmpty group)
            |> Resource.otherwise group.data
        }


      GroupSubscribedRemove subscription ->
        { group
        | removeSubscription = Resource.def subscription
        , data =
            group.addSubscription
            |> Resource.therefore (always <| dataOrEmpty group)
            |> Resource.otherwise group.data
        }

      GroupSub key subFeedback ->
        groupUpdateSub (subInput subFeedback) key group

      GroupAdd key ->
        groupAddSub subNew key group

      GroupRemove key ->
        groupRemoveSub key group

      GroupRefresh key ->
        groupUpdateSub identity key group

      GroupCancelledAdd ->
        { group
        | addSubscription =
            Resource.deriveIf Resource.isKnown (always Resource.void) group.addSubscription
            |> Resource.deriveIf Resource.isUndecided (always Resource.void)
        }

      GroupCancelledRemove ->
        { group
        | removeSubscription =
            Resource.deriveIf Resource.isKnown (always Resource.void) group.removeSubscription
            |> Resource.deriveIf Resource.isUndecided (always Resource.void)
        }

      GroupAddSubscriptionError error ->
        { group
        | addSubscription = Resource.undecided (DB.SubscriptionErrorTag error)
        , data = Resource.unknown
        , dataDelta = Dict.empty
        }

      GroupRemoveSubscriptionError error ->
        { group
        | removeSubscription = Resource.undecided (DB.SubscriptionErrorTag error)
        , data = Resource.unknown
        , dataDelta = Dict.empty
        }

      GroupReserved _ -> group


{-| Convenience concrete `DB.Data` record group update function accepting one `DB.Feedback`. -}
groupDataInputOne : GroupFeedback (DB.Feedback v) -> Group (DB.Data v) -> Group (DB.Data v)
groupDataInputOne =
  groupInputOne DB.newData DB.inputOne


{-| Group feedback update function accepting a list of `DB.Feedback`. -}
groupInput : subtype -> (subfeedback -> subtype -> subtype) -> List (GroupFeedback subfeedback) -> Group subtype -> Group subtype
groupInput subNew subInput feedbacks group =
  List.foldl (groupInputOne subNew subInput) group feedbacks


{-| Convenience concrete `DB.Data` record group update function accepting a list of `DB.Feedback`. -}
groupDataInput : List (GroupFeedback (DB.Feedback v)) -> Group (DB.Data v) -> Group (DB.Data v)
groupDataInput =
  groupInput DB.newData DB.inputOne


{-| Cancel a `Group`'s active subscription if any, resulting in a `Group` that is not effected by
subscriptions until a call to `groupReset`. -}
cancelGroup : (subtype -> (subtype, List (DB.DBTask never))) -> Group subtype -> (Group subtype, List (DB.DBTask never))
cancelGroup cancelSub group' =
  let
    cancelAdd group =
      Resource.maybeKnown group.addSubscription
      |> Maybe.map (\subs ->
        [ ElmFire.unsubscribe subs
            `andThen` (\_ -> Task.succeed ())
            `onError` (\_ -> Task.succeed ())
        ])
      |> Maybe.withDefault [ ]
      |> App.finalizeTasks App.sequence
      |> (,) { group | addSubscription = Resource.void }

    cancelRemove group =
      Resource.maybeKnown group.removeSubscription
      |> Maybe.map (\subs ->
        [ ElmFire.unsubscribe subs
            `andThen` (\_ -> Task.succeed ())
            `onError` (\_ -> Task.succeed ())
        ])
      |> Maybe.withDefault [ ]
      |> App.finalizeTasks App.sequence
      |> (,) { group | removeSubscription = Resource.void }

    cancelChildren priorGroup =
      priorGroup.dataDelta
      |> Dict.foldr
          (\key delta (group', tasks') ->
            case delta of
              (data', GroupAddD) -> (group', tasks')
              (data', _) -> cancelSub data'
                |> \(_, tasks_) -> (groupRemoveExistingData key group', tasks_ ++ tasks'))
          ({ priorGroup | dataDelta = Dict.empty }, [])
      |> \(group_, tasks_) -> Dict.foldr
          (\key data tasks' -> (snd <| cancelSub data) ++ tasks')
          tasks_ (getGroupCurrentData group_)
      |> App.finalizeTasks App.parallel
      |> (,) { group_ | data = Resource.void }

  in
    App.chainFinalizingEach App.sequence [cancelAdd, cancelRemove, cancelChildren] group'


{-| Convenience function for cancelling a `Group (DB.Data v)`. -}
cancelDataGroup : Group (DB.Data v) -> (Group (DB.Data v), List (DB.DBTask never))
cancelDataGroup =
  cancelGroup DB.cancel

{-| Reset a `Group`, such that the next time a `group*Subscriber` is reached, the `Group` will be
rebound to the given `GroupBinding`.  -}
resetGroup : Group subtype -> Group subtype
resetGroup group =
  { group
  | addSubscription =
      Resource.deriveIf Resource.isVoid (always Resource.unknown) group.addSubscription
      |> Resource.deriveIf Resource.isUndecided (always Resource.unknown)
  , removeSubscription =
      Resource.deriveIf Resource.isVoid (always Resource.unknown) group.removeSubscription
      |> Resource.deriveIf Resource.isUndecided (always Resource.unknown)
  , data =
      Resource.deriveIf Resource.isVoid (always Resource.unknown) group.data
      |> Resource.deriveIf Resource.isUndecided (always Resource.unknown)
  }


{-| Convenience function for cancelling and resetting a group at once, such that the next time
a `group*Subscriber` is reached, it will be immediately rebound. -}
cancelAndResetGroup : (subtype -> (subtype, List (DB.DBTask never))) -> Group subtype -> (Group subtype, List (DB.DBTask never))
cancelAndResetGroup cancelSub group =
  cancelGroup cancelSub group
  |> \(group', tasks) -> resetGroup group'
  |> flip (,) (App.finalizeTasks App.sequence tasks)


{-| Convenience function for cancelling and resetting a `Group (DB.Data v)` at once, such that the next
time a `group*Subscriber` is reached, it will be immediately rebound. -}
cancelAndResetDataGroup : Group (DB.Data v) -> (Group (DB.Data v), List (DB.DBTask never))
cancelAndResetDataGroup =
  cancelAndResetGroup DB.cancel


-- REMOVED IN 5.0.0
commitGroup
  :  (subtype -> (subtype, List (DB.DBTask never)))
  -> (String -> subtype -> (subtype, List (DB.DBTask never)))
  -> Group subtype -> (Group subtype, List (DB.DBTask never))
commitGroup cancelSub subscribeSub group =
  Dict.foldr
    (\key delta (group', tasks') ->
      case delta of
        (data', GroupRmD) ->
          cancelSub data'
          |> \(data_, tasks_) -> (groupRemoveExistingData key group', tasks_ ++ tasks')
        (data', deltaTag) ->
          subscribeSub key data'
          |> \(data_, tasks_) -> (groupAlwaysInsertData key data_ group', tasks_ ++ tasks')
      )
    ({ group | dataDelta = Dict.empty }, [])
    group.dataDelta
  -- parallelize subscription and cancellation of sub-items
  |> \(group'', tasks'') -> (group'', App.finalizeTasks App.parallel tasks'')


{-| This is a more configurable version of groupSubscriber. It is now the underlying code for
groupSubscriber. `groupSubscriber` is depreciated in favor of this. The rationale
for this piece is to allow total abstraction of the group source. I ran in to difficulties in
reusing group to mirror changes in local information, such as Mirrors (see DB.Mirror). -}
groupIntegrate
  :  List (GroupConfig subfeedback subbinding -> Group subtype -> (Group subtype, List (DB.DBTask never)))
  -> (subtype -> (subtype, List (DB.DBTask never)))
  -> (subbinding -> subtype -> (subtype, List (DB.DBTask never)))
  -> GroupConfig subfeedback subbinding
  -> Group subtype -> (Group subtype, List (DB.DBTask never))
groupIntegrate controllers cancelSub integrateSub config priorGroup =
  App.chain
    (commitGroup cancelSub (config.binding config.address >> integrateSub)
    :: List.map ((|>) config) controllers)
    priorGroup


{-| Controller for groupIntegrate that carries out the behavior groupSubscriber, which is the most
convenient option for mirroring data from Firebase directly. Since the previous definition was
frustratingly limited to this, I was not able to configure the group to mirror a different source,
such as bindings derived from the data in a mirror. -}
groupSubscription : ElmFire.Location -> ElmFire.OrderOptions -> GroupConfig subfeedback subbinding -> Group subtype -> (Group subtype, List (DB.DBTask never))
groupSubscription location orderOptions config priorGroup =
  let
    fromSnapshot tag snapshot =
      Signal.send config.address [tag snapshot.key]

    fromCancellation cancelTag subscriptionTag cancellation =
      case cancellation of
        ElmFire.Unsubscribed _ -> Signal.send config.address [cancelTag]
        ElmFire.QueryError _ error -> Signal.send config.address [subscriptionTag error]

    subscriptionTask (onSubs, onErr, onSnapshot, onCancellation) query' =
      (ElmFire.subscribe onSnapshot onCancellation query' location
        `andThen` (\subscription -> Signal.send config.address [onSubs subscription])
        `onError` (\error -> Signal.send config.address [onErr error]))

    (addSubscriptionTask, removeSubscriptionTask) =
      ( subscriptionTask
          ( GroupSubscribedAdd, GroupAddSubscriptionError, fromSnapshot GroupAdd
          , fromCancellation GroupCancelledAdd GroupAddSubscriptionError
          )
      , subscriptionTask
          ( GroupSubscribedRemove, GroupRemoveSubscriptionError, fromSnapshot GroupRemove
          , fromCancellation GroupCancelledRemove GroupRemoveSubscriptionError
          )
      )

  in
    App.chain
      [ App.chain
          [ App.chainIf (.removeSubscription >> Resource.isUnknown)
              [ App.doEffect (always [ removeSubscriptionTask (ElmFire.childRemoved orderOptions) ])
              , App.asEffector (\m -> { m | removeSubscription = Resource.pending })
              ]
          , App.chainIf (.addSubscription >> Resource.isUnknown)
              [ App.doEffect (always [ addSubscriptionTask (ElmFire.childAdded orderOptions) ])
              , App.asEffector (\m -> { m | addSubscription = Resource.pending })
              ]
          ]
          |> App.finalizedEffector App.sequence
      ] priorGroup
