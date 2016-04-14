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
  , GroupFeedback2
  , GroupFeedback3
  , GroupFeedback4
  , GroupConfig
  , GroupConfig2
  , GroupConfig3
  , GroupConfig4
  , Group
  , Group2
  , Group3
  , Group4

  , groupConfigBinding

  , groupConfig
  , groupConfigForward
  , groupConfigSelf
  , groupConfigSelfForward

  , groupConfigRebind
  , groupConfigRebindSelf

  , groupConfigGetAddress
  , groupConfigGetSubAddress

  , groupConfigLocation
  , groupConfigDefaultLocation
  , groupConfigMaybeLocation
  , groupConfigPathLocation
  , groupConfigInputKey
  , groupConfigPopKey
  , groupConfigTopKey
  , groupConfigGetPath
  , groupConfigSetPath
  , groupConfigNoLocation
  , groupConfigParentLocation
  , groupConfigSubLocation
  , groupConfigRootLocation
  , groupConfigHasLocation
  , groupConfigGetLocation
  , groupConfigLocationOr

  , getGroupSubFeedbackKey
  , extractGroupSubFeedbackKeys
  , getGroupSubFeedback
  , getGroupSubFeedbackPair
  , extractGroupSubFeedbackPairs

  , getGroupCurrentData
  , getGroupDeltaData
  , getGroupNextData

  , getGroupResDeltas
  , getGroupResDeltaList
  , getGroupDataResDeltas
  , getGroupDataResDeltaList
  , groupResDeltaFoldL
  , groupResDeltaFoldR
  , groupDataResDeltaFoldL
  , groupDataResDeltaFoldR
  , groupDeriveDeltaFoldL
  , groupDeriveDeltaFoldR

  , groupUpdateSub
  , groupDoSub
  , groupDoEachSub
  , groupAddSub
  , groupSetSub
  , groupRemoveSub

  , groupDeriveSub
  , getGroupSub
  , getGroupSubData

  , newGroup
  , voidGroup

  , groupInputOne
  , groupInput
  , groupDataInputOne
  , groupDataInput
  , groupInputOne2
  , groupInput2
  , groupInputOne3
  , groupInput3
  , groupInputOne4
  , groupInput4

  , resetGroup
  , cancelGroup
  , cancelDataGroup
  , cancelAndResetGroup
  , cancelGroup2
  , cancelGroup3
  , cancelGroup4
  , cancelAndResetDataGroup
  , cancelAndResetGroup2
  , cancelAndResetGroup3
  , cancelAndResetGroup4

  , flatten

  , groupIntegrate
  , groupIntegrate2
  , groupIntegrate3
  , groupIntegrate4
  , groupDataIntegrate
  , groupSubscription
  ) where

{-| Nested data. The typing is done such that one can arbitrarily nest groups, trivial data records,
or even a compatible API fitting the same pattern such that this system is easily extensible.

# Core Types
@docs GroupFeedback, GroupConfig, Group

# Nesting Convenience Types
@docs GroupFeedback2, GroupFeedback3, GroupFeedback4, GroupConfig2, GroupConfig3, GroupConfig4, Group2, Group3, Group4

# Configuration
@docs groupConfigBinding, groupConfig, groupConfigForward, groupConfigSelf, groupConfigSelfForward, groupConfigRebind, groupConfigRebindSelf, groupConfigGetAddress, groupConfigGetSubAddress, groupConfigLocation, groupConfigDefaultLocation, groupConfigMaybeLocation, groupConfigPathLocation, groupConfigInputKey, groupConfigPopKey, groupConfigTopKey, groupConfigGetPath, groupConfigSetPath, groupConfigNoLocation, groupConfigParentLocation, groupConfigSubLocation, groupConfigRootLocation, groupConfigHasLocation, groupConfigGetLocation, groupConfigLocationOr

# Inspect Subfeedback
@docs getGroupSubFeedbackKey, extractGroupSubFeedbackKeys, getGroupSubFeedback, getGroupSubFeedbackPair, extractGroupSubFeedbackPairs

# Direct Group Inquiry
@docs getGroupCurrentData, getGroupDeltaData, getGroupNextData, getGroupResDeltas, getGroupResDeltaList, getGroupDataResDeltas, getGroupDataResDeltaList, groupResDeltaFoldL, groupResDeltaFoldR, groupDataResDeltaFoldL, groupDataResDeltaFoldR, groupDeriveDeltaFoldL, groupDeriveDeltaFoldR, groupDeriveSub, getGroupSub, getGroupSubData


# Direct Group Manipulation

You can use these functions to perform nested operations on groups directly. You will need this
functionality to write new items in to a group. Take care, however, that you _do not try to remove
items form the group by using `groupRemoveSub` directly._ To effect the remote data, you must
invoke the operations provided by `Pylon.DB`.

@docs groupUpdateSub, groupDoSub, groupDoEachSub, groupAddSub, groupSetSub, groupRemoveSub

# Group Constructors
@docs newGroup, voidGroup

# Raw Group Operations

These expose the raw group interface, and all it's power. Nesting them is painstaking, though
very robust, so one might wish to use the `groupNested*` family of operations for nested groups up
up to four levels deep. For the possibility of arbitrary depth tries, please wait for Pylon 8. The
group resource tag from `Scaffold` (old version) proved to function exactly like this, with a
working public example. The tradeoff would of course be a weaker type flexibility in exchange for
much stronger structural flexibility. Expect `DynamicGroup` as part of Pylon 8. Pylon 7 will be
thoroughly documented and examples made before we get around to this.

@docs groupInputOne, groupInput, cancelGroup, resetGroup, cancelAndResetGroup, groupIntegrate

# Group Reductions

When dealing with a nested group, it can be quite a boilerplate mess to get things out dynamically.
It is much easier to do what you want with a flat group. For example. If I have a nested group
`Group (Group (Data MyRecord))` representing collections of records from a selection of different
sources in the database, I can flatten that nested group in to a regular data group `Group (Data MyRecord)`
using the `pathMerge` function to control the ordering of the results as well. I could then use a
Mirror to generate a meaningful representation and display it to the user. This can handle a LOT of
data because everything is a stream of deltas.

@docs flatten


# Data Group Operations
@docs groupDataInputOne, groupDataInput, cancelDataGroup, cancelAndResetDataGroup, groupDataIntegrate

# Nested Group Operations

Sometimes a plain group of data is not deep enough. This family of functions operates on nested
group records up to four levels deep, which should be good enough for most ordinary use cases. In
the case that you want (a) to account for a variable depth trie or (b) more levels in general,
please wait for the release of DynamicGroup with Pylon 8.

@docs groupInputOne2, groupInput2, cancelGroup2, cancelAndResetGroup2, groupIntegrate2, groupInputOne3, groupInput3, cancelGroup3, cancelAndResetGroup3, groupIntegrate3, groupInputOne4, groupInput4, cancelGroup4, cancelAndResetGroup4, groupIntegrate4

# Builtin Group Controllers

See also the `Pylon.DB.Mirror` module for a controller that manages subscriptions of sub data or
sub groups by synchronizing to a mirror.

@docs groupSubscription

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


type GroupFeedbackReserved_ =
  GFeedbackReserved_
    { reserved : ()
    }


groupFeedbackReservedNew : GroupFeedbackReserved_
groupFeedbackReservedNew = GFeedbackReserved_ { reserved = () }


{-| Feedback for nested group subscriptions. -}
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


{-| Nested group feedback, depth 2. -}
type alias GroupFeedback2 subfeedback =
  GroupFeedback (GroupFeedback subfeedback)

{-| Nested group feedback, depth 3. -}
type alias GroupFeedback3 subfeedback =
  GroupFeedback (GroupFeedback2 subfeedback)

{-| Nested group feedback, depth 4. -}
type alias GroupFeedback4 subfeedback =
  GroupFeedback (GroupFeedback3 subfeedback)


type alias GroupConfigData =
  { inputKeys_ : List String
  }


{-| Specifies the binding behavior and mailbox address for a groupIntegrate -}
type GroupConfig subfeedback subbinding =
  GroupConfig
    { binding : GroupConfig subfeedback subbinding -> String -> subbinding
    , address : Signal.Address (List (GroupFeedback subfeedback))
    , location : Maybe ElmFire.Location
    , data_ : GroupConfigData
    }


{-| Nested group configuration, depth 2. -}
type alias GroupConfig2 subfeedback subbinding =
  GroupConfig (GroupFeedback subfeedback) (GroupConfig subfeedback subbinding)

{-| Nested group configuration, depth 3. -}
type alias GroupConfig3 subfeedback subbinding =
  GroupConfig (GroupFeedback2 subfeedback) (GroupConfig2 subfeedback subbinding)

{-| Nested group configuration, depth 4. -}
type alias GroupConfig4 subfeedback subbinding =
  GroupConfig (GroupFeedback3 subfeedback) (GroupConfig3 subfeedback subbinding)


{-| Nestable group record. -}
type alias Group subtype =
  { dataDelta : Dict String (subtype, GroupDelta)
  , data : Resource DB.DBError (Dict String subtype)

  , addSubscription : Resource DB.DBError ElmFire.Subscription
  , removeSubscription : Resource DB.DBError ElmFire.Subscription

  , currentLocation : Maybe ElmFire.Location
  }


{-| Nested group record, depth 2. -}
type alias Group2 subtype =
  Group (Group (subtype))

{-| Nested group record, depth 3. -}
type alias Group3 subtype =
  Group (Group2 (subtype))

{-| Nested group record, depth 4. -}
type alias Group4 subtype =
  Group (Group3 (subtype))


{-| Interpret the current state of a `GroupConfig` by producing an immediate binding function that
passes the current state of GroupConfig as it's self parameter. See `groupConfigSelf` and
`groupConfigSelfForward` for a way to access the GroupConfig from within the binding function. -}
groupConfigBinding : GroupConfig subfeedback subbinding -> String -> subbinding
groupConfigBinding (GroupConfig config as shell) = config.binding shell


{-| TODO: write docs -}
groupConfigGetAddress : GroupConfig subfeedback subbinding -> Signal.Address (List (GroupFeedback subfeedback))
groupConfigGetAddress (GroupConfig config as shell) = config.address


computeSubAddress : String -> Signal.Address (List (GroupFeedback subfeedback)) -> Signal.Address (List subfeedback)
computeSubAddress key =
  flip Signal.forwardTo (List.map <| GroupSub key)


{-| Get the subaddress that will be used for a given key. -}
groupConfigGetSubAddress : String -> GroupConfig subfeedback subbinding -> Signal.Address (List subfeedback)
groupConfigGetSubAddress key =
  groupConfigGetAddress >> computeSubAddress key


{-| Construct a new group configuration. -}
groupConfig
  :  Signal.Address (List (GroupFeedback subfeedback))
  -> (Signal.Address (List subfeedback) -> Maybe ElmFire.Location -> String -> subbinding)
  -> GroupConfig subfeedback subbinding
groupConfig address fbinding =
  (\self key -> fbinding
      (groupConfigGetSubAddress key self)
      (groupConfigGetLocation self) key)
  |> groupConfigSelf address


{-| Construct a new self aware group configuration. -}
groupConfigSelf
  :  Signal.Address (List (GroupFeedback subfeedback))
  -> (GroupConfig subfeedback subbinding -> String -> subbinding)
  -> GroupConfig subfeedback subbinding
groupConfigSelf address fbinding =
  GroupConfig
    { address = address
    , location = Nothing
    , binding = fbinding
    , data_ = { inputKeys_ = [] }
    }

{-| Construct a new group configuration with an action forwarding function. -}
groupConfigForward
  :  (List (GroupFeedback subfeedback) -> List action)
  -> Signal.Address (List action)
  -> (Signal.Address (List subfeedback) -> Maybe ElmFire.Location -> String -> subbinding)
  -> GroupConfig subfeedback subbinding
groupConfigForward factions address =
  groupConfig (Signal.forwardTo address factions)


{-| Construct a new self aware group configuration with an action forwarding function. -}
groupConfigSelfForward
  :  (List (GroupFeedback subfeedback) -> List action)
  -> Signal.Address (List action)
  -> (GroupConfig subfeedback subbinding -> String -> subbinding)
  -> GroupConfig subfeedback subbinding
groupConfigSelfForward factions address =
  groupConfigSelf (Signal.forwardTo address factions)


{-| Rebind a group config to behave altogether differently. This is useful when propagating subbindings using the `*Self*` family. -}
groupConfigRebindSelf : (GroupConfig subfeedback subbinding' -> String -> subbinding') -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding'
groupConfigRebindSelf fbinding (GroupConfig config) =
  GroupConfig { config | binding = fbinding }


{-| Rebind a group config to behave altogether differently. This is useful when propagating subbindings using the `*Self*` family. -}
groupConfigRebind : (Signal.Address (List subfeedback) -> Maybe ElmFire.Location -> String -> subbinding') -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding'
groupConfigRebind fbinding (GroupConfig config) =
  GroupConfig
    { config
    | binding = (\self key -> fbinding
        (groupConfigGetSubAddress key self)
        (groupConfigGetLocation self) key)
    }



{-| Set a location for this group configuration. Note that since the Location type is opaque to
Pylon and is "generally not validated" (via the ElmFire docs), it is not possible for us to ensure
that the current location always matches up with the current path. Please take care. -}
groupConfigLocation : ElmFire.Location -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigLocation location (GroupConfig config as shell) =
  GroupConfig
    { config
    | location = Just location
    }


{-| Set a location if it exists. -}
groupConfigMaybeLocation : Maybe ElmFire.Location -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigMaybeLocation location shell =
  Maybe.map (\loc -> groupConfigLocation loc shell) location
  |> Maybe.withDefault shell


{-| Set a location for this group configuration by location and path. The root location will be
derived from the given location (you should give the root location anyway for clarity's sake) and
then groupConfigSubLocation will be applied to each path element starting from the left, resulting
in a properly rooted full path. This can be manipulated using groupConfigParentLocation with
valid results. -}
groupConfigPathLocation : List String -> ElmFire.Location -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigPathLocation path location =
  groupConfigLocation location
  >> groupConfigRootLocation
  >> flip (List.foldl groupConfigSubLocation) path


{-| TODO: write docs -}
groupConfigDefaultLocation : ElmFire.Location -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigDefaultLocation location (GroupConfig config as shell) =
  if config.location /= Nothing then
    shell
  else
    GroupConfig
      { config
      | location = Just location
      }


{-| TODO: write docs -}
groupConfigInputKey : String -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigInputKey key (GroupConfig config) =
  let data_ = config.data_ in
    GroupConfig
      { config
      | data_ = { data_ | inputKeys_ = key :: data_.inputKeys_ }
      }


{-| TODO: write docs -}
groupConfigPopKey : GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigPopKey (GroupConfig config as shell) =
  let data_ = config.data_ in
    Maybe.map
      (\ls' ->
        GroupConfig
          { config
          | data_ = { data_ | inputKeys_ = ls' }
          }
      ) (List.tail data_.inputKeys_)
    |> Maybe.withDefault shell


{-| TODO: write docs -}
groupConfigTopKey : GroupConfig subfeedback subbinding -> Maybe String
groupConfigTopKey (GroupConfig config as shell) = List.head config.data_.inputKeys_


{-| TODO: write docs -}
groupConfigGetPath : GroupConfig subfeedback subbinding -> List String
groupConfigGetPath (GroupConfig config as shell) = List.reverse config.data_.inputKeys_


{-| TODO: write docs -}
groupConfigSetPath : List String -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigSetPath path (GroupConfig config as shell) =
  let data_ = config.data_ in
    GroupConfig
      { config
      | data_ = { data_ | inputKeys_ = List.reverse path }
      }


-- does not effect keys.
{-| TODO: write docs -}
groupConfigNoLocation : GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigNoLocation (GroupConfig config as shell) =
  if config.location == Nothing then
    shell
  else
    GroupConfig
      { config
      | location = Nothing
      }

-- parent location will not work for a non-relative path, however, rooting everything will
-- effectively reset to a valid state.
{-| TODO: write docs -}
groupConfigParentLocation : GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigParentLocation (GroupConfig config as shell) =
  case Maybe.map ElmFire.root config.location of
    Nothing -> groupConfigPopKey shell
    Just root ->
      GroupConfig
        { config
        | location =
            Maybe.map ElmFire.root config.location
            |> Maybe.map (flip (List.foldr ElmFire.sub) config.data_.inputKeys_)
        }
      |> groupConfigPopKey


{-| TODO: write docs -}
groupConfigSubLocation : String -> GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigSubLocation key (GroupConfig config as shell) =
  let data_ = config.data_ in
    GroupConfig
      { config
      | location = Maybe.map (ElmFire.sub key) config.location
      , data_ = { data_ | inputKeys_ = key :: data_.inputKeys_ }
      }


{-| TODO: write docs -}
groupConfigRootLocation : GroupConfig subfeedback subbinding -> GroupConfig subfeedback subbinding
groupConfigRootLocation (GroupConfig config as shell) =
  let data_ = config.data_ in
    GroupConfig
      { config
      | location = Maybe.map ElmFire.root config.location
      , data_ =  { data_ | inputKeys_ = [] }
      }


{-| TODO: write docs -}
groupConfigHasLocation : GroupConfig subfeedback subbinding -> Bool
groupConfigHasLocation (GroupConfig config as shell) =
  config.location /= Nothing


{-| TODO: write docs -}
groupConfigGetLocation : GroupConfig subfeedback subbinding -> Maybe ElmFire.Location
groupConfigGetLocation (GroupConfig config as shell) =
  config.location


{-| TODO: write docs -}
groupConfigLocationOr : GroupConfig subfeedback subbinding -> ElmFire.Location -> ElmFire.Location
groupConfigLocationOr (GroupConfig config as shell) =
  flip Maybe.withDefault config.location




-- INQUIRE ABOUT GROUP FEEDBACK


{-| Get the key of the subfeedback in a given GroupFeedback if applicable. -}
getGroupSubFeedbackKey : GroupFeedback subfeedback -> Maybe String
getGroupSubFeedbackKey feedback =
  case feedback of
    GroupAdd key -> Just key
    GroupRemove key -> Just key
    GroupRefresh key -> Just key
    GroupSub key _ -> Just key
    _ -> Nothing


{-| Extract all unique keys from a list of GroupFeedbacks. -}
extractGroupSubFeedbackKeys : List (GroupFeedback subfeedback) -> List String
extractGroupSubFeedbackKeys =
  List.foldr
    (\feedback ((list, set) as pair) ->
      getGroupSubFeedbackKey feedback
      |> Maybe.map
          (\key ->
            if Set.member key set then pair
            else (key :: list, Set.insert key set))
      |> Maybe.withDefault pair
    ) ([], Set.empty) >> fst


{-| Get the subfeedback in a given GroupFeedback if applicable. -}
getGroupSubFeedback : GroupFeedback subfeedback -> Maybe subfeedback
getGroupSubFeedback feedback =
  case feedback of
    GroupSub _ feedback' -> Just feedback'
    _ -> Nothing


{-| Get the subfeedback and it's associated key `(key, feedback')` in a given GroupFeedback if applicable. -}
getGroupSubFeedbackPair : GroupFeedback subfeedback -> Maybe (String, subfeedback)
getGroupSubFeedbackPair feedback =
  case feedback of
    GroupSub key feedback' -> Just (key, feedback')
    _ -> Nothing


{-| Extract all `(key, feedback')` pairs from a list of GroupFeedbacks. -}
extractGroupSubFeedbackPairs : List (GroupFeedback subfeedback) -> List (String, subfeedback)
extractGroupSubFeedbackPairs =
  List.filterMap getGroupSubFeedbackPair


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


{-| Get what the group's data will be after the deltas are applied. Note that this result does not
account for cancellation and integration actions that will be applied by groupIntegrate. -}
getGroupNextData : Group subtype -> Dict String subtype
getGroupNextData group =
  Dict.foldl
    (\key (data', deltaTag) ->
      if deltaTag == GroupRmD then
        Dict.remove key
      else
        Dict.insert key data'
    ) (getGroupCurrentData group) group.dataDelta


{-| Get the current change in the group's data as a dictionary of resource pairs, each representing
the prior and current values of the data respectively. -}
getGroupDataResDeltas : Group (DB.Data v) -> Dict String (Resource DB.DBError v, Resource DB.DBError v)
getGroupDataResDeltas =
  groupDataResDeltaFoldL Dict.insert Dict.empty


{-| Get the current change in the group's data as a list of (key, (prior, current)) structures. -}
getGroupDataResDeltaList : Group (DB.Data v) -> List (String, (Resource DB.DBError v, Resource DB.DBError v))
getGroupDataResDeltaList =
  groupDataResDeltaFoldR (\key pair list -> (key, pair) :: list) []


{-| Get the current change in the group's data as a dictionary of resource pairs, each representing
the prior and current values of the data respectively. -}
getGroupResDeltas : Group subtype -> Dict String (Resource DB.DBError subtype, Resource DB.DBError subtype)
getGroupResDeltas =
  groupResDeltaFoldL Dict.insert Dict.empty


{-| Get the current change in the group's data as a list of (key, (prior, current)) structures. -}
getGroupResDeltaList : Group subtype -> List (String, (Resource DB.DBError subtype, Resource DB.DBError subtype))
getGroupResDeltaList =
  groupResDeltaFoldR (\key pair list -> (key, pair) :: list) []


{-| Fold from the left across the deltas for a data group, using a particular fold function and initial output. -}
groupDataResDeltaFoldL : (String -> (Resource DB.DBError v, Resource DB.DBError v) -> foldout -> foldout) -> foldout -> Group (DB.Data v) -> foldout
groupDataResDeltaFoldL = groupAnyResDeltaFold False .value


{-| Fold from the right across the deltas for a data group, using a particular fold function and initial output. -}
groupDataResDeltaFoldR : (String -> (Resource DB.DBError v, Resource DB.DBError v) -> foldout -> foldout) -> foldout -> Group (DB.Data v) -> foldout
groupDataResDeltaFoldR = groupAnyResDeltaFold True .value


{-| Fold from the left across the deltas, using a particular fold function and initial output. -}
groupResDeltaFoldL : (String -> (Resource DB.DBError subtype, Resource DB.DBError subtype) -> foldout -> foldout) -> foldout -> Group subtype -> foldout
groupResDeltaFoldL = groupAnyResDeltaFold False Resource.def


{-| Fold from the right across the deltas, using a particular fold function and initial output. -}
groupResDeltaFoldR : (String -> (Resource DB.DBError subtype, Resource DB.DBError subtype) -> foldout -> foldout) -> foldout -> Group subtype -> foldout
groupResDeltaFoldR = groupAnyResDeltaFold True Resource.def


{-| Fold from the left across the deltas, given a given derivation function and using a particular fold function and initial output. -}
groupDeriveDeltaFoldL : (subtype -> Resource DB.DBError v) -> (String -> (Resource DB.DBError v, Resource DB.DBError v) -> foldout -> foldout) -> foldout -> Group subtype -> foldout
groupDeriveDeltaFoldL f = groupAnyResDeltaFold False f


{-| Fold from the right across the deltas, given a given derivation function and using a particular fold function and initial output. -}
groupDeriveDeltaFoldR : (subtype -> Resource DB.DBError v) -> (String -> (Resource DB.DBError v, Resource DB.DBError v) -> foldout -> foldout) -> foldout -> Group subtype -> foldout
groupDeriveDeltaFoldR f = groupAnyResDeltaFold True f


groupAnyResDeltaFold : Bool -> (subtype -> Resource DB.DBError v) -> (String -> (Resource DB.DBError v, Resource DB.DBError v) -> foldout -> foldout) -> foldout -> Group subtype -> foldout
groupAnyResDeltaFold fromRight extractValueResource fFold foldIn group =
  let
    foldOp = if fromRight then Dict.foldr else Dict.foldl

    priorData = getGroupCurrentData group

    priorValueOf key =
      case Dict.get key priorData of
        Just prior -> extractValueResource prior
        Nothing -> Resource.void

    deltaPairOf key (data', deltaTag) foldOut =
      (case deltaTag of
        GroupRmD  -> (priorValueOf key, Resource.void)
        GroupSubD -> (priorValueOf key, extractValueResource data')
        GroupAddD -> (priorValueOf key, extractValueResource data'))
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
getGroupSub : String -> Group subtype -> Maybe subtype
getGroupSub = groupDeriveSub identity


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


{-| This is for directly setting a sub item's current value in the group. This should be used in
processing of derived groups that are not associated with a running controller from a GroupConfig.
-}
groupSetSub : String -> subtype -> Group subtype -> Group subtype
groupSetSub key subItem group =
  case Dict.get key group.dataDelta of
    Just (data', GroupRmD) -> { group | dataDelta = Dict.insert key (subItem, GroupSubD) group.dataDelta }
    Just (data', GroupSubD) -> { group | dataDelta = Dict.insert key (subItem, GroupSubD) group.dataDelta }
    Just (data', GroupAddD) -> { group | dataDelta = Dict.insert key (subItem, GroupAddD) group.dataDelta }

    Nothing ->
      Dict.get key (getGroupCurrentData group)
      |> Maybe.map (\_ -> { group | dataDelta = Dict.insert key (subItem, GroupSubD) group.dataDelta })
      |> Maybe.withDefault { group | dataDelta = Dict.insert key (subItem, GroupAddD) group.dataDelta }



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


-- CONSTRUCTORS

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



-- HANDLING SIGNAL FEEDBACK

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


{-| groupInputOne for depth 2. -}
groupInputOne2 : subtype -> (subfeedback -> subtype -> subtype) -> GroupFeedback2 subfeedback -> Group2 subtype -> Group2 subtype
groupInputOne2 newSub inputSub =
  groupInputOne newSub inputSub
  |> groupInputOne newGroup


{-| groupInputOne for depth 3. -}
groupInputOne3 : subtype -> (subfeedback -> subtype -> subtype) -> GroupFeedback3 subfeedback -> Group3 subtype -> Group3 subtype
groupInputOne3 newSub inputSub =
  groupInputOne2 newSub inputSub
  |> groupInputOne newGroup


{-| groupInputOne for depth 4. -}
groupInputOne4 : subtype -> (subfeedback -> subtype -> subtype) -> GroupFeedback4 subfeedback -> Group4 subtype -> Group4 subtype
groupInputOne4 newSub inputSub =
  groupInputOne3 newSub inputSub
  |> groupInputOne newGroup


{-| Group feedback update function accepting a list of `DB.Feedback`. -}
groupInput : subtype -> (subfeedback -> subtype -> subtype) -> List (GroupFeedback subfeedback) -> Group subtype -> Group subtype
groupInput subNew subInput feedbacks group =
  List.foldl (groupInputOne subNew subInput) group feedbacks


{-| Convenience concrete `DB.Data` record group update function accepting a list of `DB.Feedback`. -}
groupDataInput : List (GroupFeedback (DB.Feedback v)) -> Group (DB.Data v) -> Group (DB.Data v)
groupDataInput =
  groupInput DB.newData DB.inputOne


{-| groupInput for depth 2. -}
groupInput2 : subtype -> (subfeedback -> subtype -> subtype) -> List (GroupFeedback2 subfeedback) -> Group2 subtype -> Group2 subtype
groupInput2 newSub inputSub =
  groupInputOne newSub inputSub
  |> groupInput newGroup


{-| groupInput for depth 3. -}
groupInput3 : subtype -> (subfeedback -> subtype -> subtype) -> List (GroupFeedback3 subfeedback) -> Group3 subtype -> Group3 subtype
groupInput3 newSub inputSub =
  groupInputOne2 newSub inputSub
  |> groupInput newGroup


{-| groupInput for depth 4. -}
groupInput4 : subtype -> (subfeedback -> subtype -> subtype) -> List (GroupFeedback4 subfeedback) -> Group4 subtype -> Group4 subtype
groupInput4 newSub inputSub =
  groupInputOne3 newSub inputSub
  |> groupInput newGroup


-- CANCEL AND RESET


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

{-| cancelGroup for depth 2. -}
cancelGroup2 : (subtype -> (subtype, List (DB.DBTask never))) -> Group2 subtype -> (Group2 subtype, List (DB.DBTask never))
cancelGroup2 cancelSub =
  cancelGroup cancelSub
  |> cancelGroup


{-| cancelGroup for depth 3. -}
cancelGroup3 : (subtype -> (subtype, List (DB.DBTask never))) -> Group3 subtype -> (Group3 subtype, List (DB.DBTask never))
cancelGroup3 cancelSub =
  cancelGroup2 cancelSub
  |> cancelGroup


{-| cancelGroup for depth 4. -}
cancelGroup4 : (subtype -> (subtype, List (DB.DBTask never))) -> Group4 subtype -> (Group4 subtype, List (DB.DBTask never))
cancelGroup4 cancelSub =
  cancelGroup3 cancelSub
  |> cancelGroup


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


{-| cancelAndResetGroup for depth 2. -}
cancelAndResetGroup2 : (subtype -> (subtype, List (DB.DBTask never))) -> Group2 subtype -> (Group2 subtype, List (DB.DBTask never))
cancelAndResetGroup2 cancelSub =
  App.chain
    [ App.chain
        [ cancelGroup2 cancelSub
        , App.asEffector resetGroup
        ]
        |> App.finalizedEffector App.sequence
    ]

{-| cancelAndResetGroup for depth 3. -}
cancelAndResetGroup3 : (subtype -> (subtype, List (DB.DBTask never))) -> Group3 subtype -> (Group3 subtype, List (DB.DBTask never))
cancelAndResetGroup3 cancelSub =
  App.chain
    [ App.chain
        [ cancelGroup3 cancelSub
        , App.asEffector resetGroup
        ]
        |> App.finalizedEffector App.sequence
    ]

{-| cancelAndResetGroup for depth 4. -}
cancelAndResetGroup4 : (subtype -> (subtype, List (DB.DBTask never))) -> Group4 subtype -> (Group4 subtype, List (DB.DBTask never))
cancelAndResetGroup4 cancelSub =
  App.chain
    [ App.chain
        [ cancelGroup4 cancelSub
        , App.asEffector resetGroup
        ]
        |> App.finalizedEffector App.sequence
    ]



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


-- FLATTENING


{-| Flatten a nested group `Group (Group subtype)` to a group `Group subtype` using a path
reduction. For example if pathMerge were

    \outer inner -> outer ++ "/" ++ inner

then the resulting keys would be path fragments such as `foo/bar` where `foo` is the key of the
outer group and `bar` is the key of the inner group.

-}
flatten : (String -> String -> String) -> Group (Group subtype) -> Group subtype -> Group subtype
flatten pathMerge deepGroup group =
  groupResDeltaFoldL
    (\key subPair group' ->
      case subPair of
        (Resource.Known _, Resource.Known subGroup) ->
          groupResDeltaFoldL
            (\key' subSubPair ->
              case subSubPair of
                (_, Resource.Known sub) -> groupSetSub (pathMerge key key') sub
                (Resource.Known _, _) -> groupRemoveSub (pathMerge key key')
                (_, _) -> identity
            ) group' subGroup

        (_, Resource.Known subGroup) ->
          Dict.foldl (pathMerge key >> groupSetSub) group' (getGroupNextData subGroup)

        (Resource.Known subGroup, _) ->
          Dict.foldl
            (\key' _ -> pathMerge key key'
            |> groupRemoveSub)
            group' (getGroupNextData subGroup)

        (_, _) -> group'
    ) group deepGroup


-- INTEGRATION


{-| Convenient short version for groups of just data. This equivalency holds:

    groupDataIntegrate controllers =
      groupIntegrate controllers DB.cancel DB.subscribe

-}
groupDataIntegrate
  :  List (GroupConfig (DB.Feedback v) (DB.Binding v) -> Group (DB.Data v) -> (Group (DB.Data v), List (DB.DBTask never)))
  -> GroupConfig (DB.Feedback v) (DB.Binding v)
  -> Group (DB.Data v) -> (Group (DB.Data v), List (DB.DBTask never))
groupDataIntegrate controllers =
  groupIntegrate controllers DB.cancel DB.subscribe


{-| Integrate a group using the given controller functions, a configuration with the sub-binding
function, and the address to send GroupFeedback to.

This is a more configurable version of groupSubscriber. It is now the underlying code for
groupSubscriber. `groupSubscriber` is depreciated in favor of this, and can be found in
`Pylon.Legacy.Group`. -}
groupIntegrate
  :  List (GroupConfig subfeedback subbinding -> Group subtype -> (Group subtype, List (DB.DBTask never)))

  -> (subtype -> (subtype, List (DB.DBTask never)))
  -> (subbinding -> subtype -> (subtype, List (DB.DBTask never)))

  -> GroupConfig subfeedback subbinding
  -> Group subtype
  -> (Group subtype, List (DB.DBTask never))
groupIntegrate controllers cancelSub integrateSub config priorGroup =
  App.chain
    (commitGroup cancelSub (groupConfigBinding config >> integrateSub)
    :: List.map ((|>) config) controllers)
    priorGroup

{-| groupIntegrate for depth 2. -}
groupIntegrate2
  :  List (GroupConfig2 subfeedback subbinding -> Group2 subtype -> (Group2 subtype, List (DB.DBTask never)))
  -> List (GroupConfig subfeedback subbinding -> Group subtype -> (Group subtype, List (DB.DBTask never)))

  -> (subtype -> (subtype, List (DB.DBTask never)))
  -> (subbinding -> subtype -> (subtype, List (DB.DBTask never)))

  -> GroupConfig2 subfeedback subbinding
  -> Group2 subtype
  -> (Group2 subtype, List (DB.DBTask never))
groupIntegrate2 ctrl2 ctrl1 subCancel subIntegrate =
  groupIntegrate
    ctrl2
    (cancelGroup subCancel)
    (groupIntegrate ctrl1 subCancel subIntegrate)

{-| groupIntegrate for depth 3. -}
groupIntegrate3
  :  List (GroupConfig3 subfeedback subbinding -> Group3 subtype -> (Group3 subtype, List (DB.DBTask never)))
  -> List (GroupConfig2 subfeedback subbinding -> Group2 subtype -> (Group2 subtype, List (DB.DBTask never)))
  -> List (GroupConfig subfeedback subbinding -> Group subtype -> (Group subtype, List (DB.DBTask never)))

  -> (subtype -> (subtype, List (DB.DBTask never)))
  -> (subbinding -> subtype -> (subtype, List (DB.DBTask never)))

  -> GroupConfig3 subfeedback subbinding
  -> Group3 subtype
  -> (Group3 subtype, List (DB.DBTask never))
groupIntegrate3 ctrl3 ctrl2 ctrl1 subCancel subIntegrate =
  groupIntegrate2
    ctrl3
    ctrl2
    (cancelGroup subCancel)
    (groupIntegrate ctrl1 subCancel subIntegrate)

{-| groupIntegrate for depth 4. -}
groupIntegrate4
  :  List (GroupConfig4 subfeedback subbinding -> Group4 subtype -> (Group4 subtype, List (DB.DBTask never)))
  -> List (GroupConfig3 subfeedback subbinding -> Group3 subtype -> (Group3 subtype, List (DB.DBTask never)))
  -> List (GroupConfig2 subfeedback subbinding -> Group2 subtype -> (Group2 subtype, List (DB.DBTask never)))
  -> List (GroupConfig subfeedback subbinding -> Group subtype -> (Group subtype, List (DB.DBTask never)))

  -> (subtype -> (subtype, List (DB.DBTask never)))
  -> (subbinding -> subtype -> (subtype, List (DB.DBTask never)))

  -> GroupConfig4 subfeedback subbinding
  -> Group4 subtype
  -> (Group4 subtype, List (DB.DBTask never))
groupIntegrate4 ctrl4 ctrl3 ctrl2 ctrl1 subCancel subIntegrate =
  groupIntegrate3
    ctrl4
    ctrl3
    ctrl2
    (cancelGroup subCancel)
    (groupIntegrate ctrl1 subCancel subIntegrate)


{-
groupInputOne
groupInput

cancelGroup
cancelAndResetGroup

groupNestedIntegrate
-}


{-| Controller for groupIntegrate that carries out the behavior groupSubscriber, which is the most
convenient option for mirroring data from Firebase directly. Since the previous definition was
frustratingly limited to this, I was not able to configure the group to mirror a different source,
such as bindings derived from the data in a mirror. -}
groupSubscription
  :  ElmFire.OrderOptions
  -> GroupConfig subfeedback subbinding
  -> Group subtype
  -> (Group subtype, List (DB.DBTask never))
groupSubscription orderOptions (GroupConfig config as shell) priorGroup =
  case config.location of
    Just location ->
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
              [ App.asEffector (\m -> { m | currentLocation = Just location })
              , App.chainIf (.removeSubscription >> Resource.isUnknown)
                  [ App.doEffect (always [ removeSubscriptionTask (ElmFire.childRemoved orderOptions) ])
                  , App.asEffector (\m -> { m | removeSubscription = Resource.pending })
                  ]
              , App.chainIf (.addSubscription >> Resource.isUnknown)
                  [ App.doEffect (always [ addSubscriptionTask (ElmFire.childAdded orderOptions) ])
                  , App.asEffector (\m -> { m | addSubscription = Resource.pending })
                  ]
              ]
              |> App.finalizedEffector App.parallel
          ] priorGroup

    Nothing ->
      App.chain
        [ App.asEffector (\m -> { m | currentLocation = Nothing })
        , App.asEffector (Debug.log "Running subscription, but no location provided!")
        ] priorGroup
