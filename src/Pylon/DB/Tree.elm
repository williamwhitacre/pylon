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


module Pylon.DB.Tree
  ( Tree
  , TreeFeedback(..)
  , Context
  , Config
  , Controller

  , context
  , contextForward
  , contextSetRoot
  , pathContext
  , contextPath
  , locationContext
  , contextLocation
  , contextPrefixSet
  , contextPrefixGet

  , dbConfig

  , controllerSubscribe
  , controllerManual
  , controllerData
  , controllerSubscribeFrom
  , controllerManualFrom
  , controllerDataFrom

  , feedbackPath

  , newTree
  , cancel
  , cancelAndReset
  , subscribe
  , addPath
  , removePath
  , inputOne
  , step
  ) where


{-| This module is intended to mirror entire trees of data on firebase.

# Types
@docs Tree, TreeFeedback, Context, Config, Controller

# Manipulate Context
@docs context, contextForward, contextSetRoot, pathContext, contextPath, locationContext, contextLocation, contextPrefixSet, contextPrefixGet

# Controller Policies
@docs controllerSubscribe, controllerManual, controllerData, controllerSubscribeFrom, controllerManualFrom, controllerDataFrom

# Default Configuration
@docs dbConfig

# Feedback Helpers
@docs feedbackPath

# Tree
@docs newTree, cancel, cancelAndReset, subscribe, addPath, removePath, inputOne, step

-}


import Pylon.App as App
import Pylon.Resource as Resource exposing (Resource)
import Pylon.DB as DB
import Pylon.DB.Path as Path exposing (Path)

import Trampoline exposing (..)

import ElmFire

import Task exposing (Task, andThen, onError)
import Dict exposing (Dict)


type alias ReservedType = ()


type alias Children subtype =
  { children : Dict String (TreeNode subtype)
  , next : Dict String (TreeNode subtype)
  , delta : Dict String (Maybe (TreeNode subtype), Maybe (TreeNode subtype))
  }

type alias Listener =
  { addSubs : Resource DB.DBError ElmFire.Subscription
  , removeSubs : Resource DB.DBError ElmFire.Subscription
  }

{-| Specifies the controller method to use for a given node. -}
type Controller =
  Auto ElmFire.OrderOptions
  | Manual
  | Data
  | AutoRebase Path ElmFire.OrderOptions
  | ManualRebase Path
  | DataRebase Path


{-| -}
controllerSubscribe : ElmFire.OrderOptions -> Controller
controllerSubscribe = Auto

{-| -}
controllerManual : Controller
controllerManual = Manual

{-| -}
controllerData : Controller
controllerData = Data

{-| -}
controllerSubscribeFrom : Path -> ElmFire.OrderOptions -> Controller
controllerSubscribeFrom = AutoRebase

{-| -}
controllerManualFrom : Path -> Controller
controllerManualFrom = ManualRebase

{-| -}
controllerDataFrom : Path -> Controller
controllerDataFrom = DataRebase


{-| Feedback type for a DB.Tree. -}
type TreeFeedback subfeedback =
  FromSub Path subfeedback

  | AddSub Path
  | RemoveSub Path

  | ListenAddStart Path ElmFire.Subscription
  | ListenAddCancelled Path
  | ListenAddCancelledReset Path
  | ListenAddError Path DB.DBError

  | ListenRemoveStart Path ElmFire.Subscription
  | ListenRemoveCancelled Path
  | ListenRemoveCancelledReset Path
  | ListenRemoveError Path DB.DBError

  | ReservedFeedback Path ReservedType


type alias NodeMeta =
  { depth : Int
  , size : Int
  , listener : Listener
  , targetPath : Path
  , errors : List DB.DBError
  , switchController : Bool
  , cancelOnRemove : Bool
  }

type TreeNode subtype =
  Node NodeMeta (Children subtype)
  | Leaf NodeMeta subtype
  | Stub NodeMeta

{-| A single tree. The tree represents an active record in the database in conjunction with a
configuration (see `Config`) and some context (see `Context`). -}
type Tree subtype =
  Tree
    { root : TreeNode subtype
    }


{-| Configuration type for the tree. Configures the subscription, cancellation, input, and
initialization functions for the underlying subtype. Takes a policy. This allows you to control the
subscription behavior of each node, selecting whether it should be interpreted as a group node or a
leaf node, and whether or not it should listen for children.
-}
type alias Config never subtype subfeedback =
  { subscription : Context subfeedback -> Path -> Path -> subtype -> (subtype, List (DB.DBTask never))
  , cancellation : Context subfeedback -> Path -> Path -> subtype -> (subtype, List (DB.DBTask never))
  , subInput : Path -> subfeedback -> subtype -> subtype
  , subInit : Path -> subtype
  , policy : Path -> Controller
  }


{-| Specify some context for the tree. This includes the address, the root location of the database,
and the prefix path to use. The prefix selects the location in the database to mirror. -}
type alias Context subfeedback =
  { address : Signal.Address (List (TreeFeedback subfeedback))
  , rootLocation : ElmFire.Location
  , prefixPath : Path
  }


--_-----------------_--
----- CONTEXT API -----
--`-----------------`--


{-| Create a new context given an address and a location. This defaults to an empty prefix, which
will bind the active record at the root database location. -}
context : Signal.Address (List (TreeFeedback subfeedback)) -> ElmFire.Location -> Context subfeedback
context address rootLocation =
  { address = address
  , rootLocation = rootLocation
  , prefixPath = Path.base
  }

{-| Create a new context, but with a forwarded address. -}
contextForward : (List (TreeFeedback subfeedback) -> List action) -> Signal.Address (List action) -> ElmFire.Location -> Context subfeedback
contextForward factions address =
  context (Signal.forwardTo address factions)


{-| Set the root database location. -}
contextSetRoot : ElmFire.Location -> Context subfeedback -> Context subfeedback
contextSetRoot rootLocation context =
  { context
  | rootLocation = rootLocation
  }


{-| Determine the full path of a given child path from context, putting the path argument last. -}
pathContext : Context subfeedback -> Path -> Path
pathContext =
  flip contextPath


{-| Determine the full path of a given child path from context, putting the context argument last. -}
contextPath : Path -> Context subfeedback -> Path
contextPath path =
  .prefixPath >> Path.childPath path


{-| Determine the location of a given child path from context, putting the path argument last. -}
locationContext : Context subfeedback -> Path -> ElmFire.Location
locationContext =
  flip contextLocation


{-| Determine the location of a given child path from context, putting the context argument last. -}
contextLocation : Path -> Context subfeedback -> ElmFire.Location
contextLocation path context =
  contextPath path context
  |> Path.absoluteLocation context.rootLocation


{-| Set the prefix path in the context. -}
contextPrefixSet : Path -> Context subfeedback -> Context subfeedback
contextPrefixSet path context =
  { context
  | prefixPath = path
  }


{-| Get the prefix path in the context. -}
contextPrefixGet : Context subfeedback -> Path
contextPrefixGet =
  .prefixPath


--_--------------------_--
----- DEFAULT CONFIG -----
--`--------------------`--


{-| This is the default configuration designed to work with `DB.Data`. Alternative machines may be
preferred for odd tasks. -}
dbConfig : DB.Config v -> (Path -> Controller) -> Config never (DB.Data v) (DB.Feedback v)
dbConfig config policy =
  { subscription =
      (\context localPath -> locationContext context
      >> DB.bindingFrom config
      >> DB.forwardingTo (List.map <| FromSub localPath) context.address
      >> DB.subscribe)
  , cancellation =
      (\_ _ _ -> DB.cancel)
  , subInput =
      (always DB.inputOne)
  , subInit =
      always DB.newData
  , policy = policy
  }


--_--------------_--
----- META API -----
--`--------------`--


metaDefault : NodeMeta
metaDefault =
  { depth = 0
  , size = 0
  , listener = listenerEmpty
  , targetPath = Path.base
  , errors = []
  , switchController = False
  , cancelOnRemove = False
  }


--_-------------------------_--
----- CHILDREN STRUCT API -----
--`-------------------------`--


childrenEmpty : Children subtype
childrenEmpty =
  { children = Dict.empty
  , next = Dict.empty
  , delta = Dict.empty
  }


childrenSetCancel : String -> Children subtype -> Children subtype
childrenSetCancel key =
  childrenSet' True key Nothing


childrenSet : String -> Maybe (TreeNode subtype) -> Children subtype -> Children subtype
childrenSet =
  childrenSet' False


childrenClearCancel : Children subtype -> Children subtype
childrenClearCancel =
  childrenClear' True


childrenClear : Children subtype -> Children subtype
childrenClear =
  childrenClear' False


childrenClear' : Bool -> Children subtype -> Children subtype
childrenClear' bDoCancellation childrenStruct =
  Dict.foldl
    (\key next -> if bDoCancellation then
      childrenSetCancel key
    else
      childrenSet key Nothing)
    childrenStruct
    childrenStruct.next


childrenSet' : Bool -> String -> Maybe (TreeNode subtype) -> Children subtype -> Children subtype
childrenSet' bCancelOnRemove key mnode childrenStruct =
  let
    filterResultPair pair =
      case pair of
        (Nothing, Nothing) -> Nothing
        _ -> Just pair

    deduceResultPair =
      Maybe.map (\(prior, next) -> (prior, mnode))
      >> Maybe.withDefault (Dict.get key childrenStruct.children, mnode)
      >> (\(prior, next) ->
          (Maybe.map (nodeSetCancelOnRemove bCancelOnRemove) prior, next))

  in
    { childrenStruct
    | next =
        case mnode of
          Just mnode' -> Dict.insert key mnode' childrenStruct.next
          Nothing -> Dict.remove key childrenStruct.next

    , delta =
        Dict.update key
          (deduceResultPair >> filterResultPair)
          childrenStruct.delta
    }


childrenStep : Children subtype -> Children subtype
childrenStep childrenStruct =
  { childrenStruct
  | children =
      Dict.foldl
        (\key pair next' ->
          case pair of
            (_, Just mnode) -> Dict.insert key (nodeStep mnode) next'
            (_, _) -> next'
        )
        childrenStruct.next
        childrenStruct.delta
  , delta = Dict.empty
  }


childrenGetCurrent : String -> Children subtype -> Maybe (TreeNode subtype)
childrenGetCurrent key =
  .children >> Dict.get key


childrenGetNext : String -> Children subtype -> Maybe (TreeNode subtype)
childrenGetNext key =
  .next >> Dict.get key


childrenGetDelta : String -> Children subtype -> (Maybe (TreeNode subtype), Maybe (TreeNode subtype))
childrenGetDelta key children =
  Dict.get key children.delta
  |> Maybe.withDefault (Nothing, Nothing)


childrenFoldDeltaLeft : (String -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> outtype -> outtype) -> outtype -> Children subtype -> outtype
childrenFoldDeltaLeft =
  childrenFoldDelta' False


childrenFoldDeltaRight : (String -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> outtype -> outtype) -> outtype -> Children subtype -> outtype
childrenFoldDeltaRight =
  childrenFoldDelta' True


childrenFoldDelta' : Bool -> (String -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> outtype -> outtype) -> outtype -> Children subtype -> outtype
childrenFoldDelta' bFromRight f out {delta} =
  (if bFromRight then Dict.foldr else Dict.foldl)
    (\key -> uncurry (f key)) out delta


--_------------------_--
----- LISTENER API -----
--`------------------`--


listenerEmpty : Listener
listenerEmpty =
  { addSubs = Resource.unknown
  , removeSubs = Resource.unknown
  }


writeListenAdd : Resource DB.DBError ElmFire.Subscription -> Listener -> Listener
writeListenAdd subs listener =
  { listener | addSubs = subs }

readListenAdd : Listener -> Resource DB.DBError ElmFire.Subscription
readListenAdd = .addSubs

writeListenRemove : Resource DB.DBError ElmFire.Subscription -> Listener -> Listener
writeListenRemove subs listener =
  { listener | removeSubs = subs }

readListenRemove : Listener -> Resource DB.DBError ElmFire.Subscription
readListenRemove = .removeSubs


--_------------------_--
----- FEEDBACK API -----
--`------------------`--

{-| Extract the path of the given feedback. All feedback tags have an associated path in the tree.
Use this to get it without case destructuring. -}
feedbackPath : TreeFeedback subfeedback -> Path
feedbackPath feedback =
  case feedback of
    FromSub path _ -> path
    AddSub path -> path
    RemoveSub path -> path
    ListenAddStart path _ -> path
    ListenAddCancelled path -> path
    ListenAddCancelledReset path -> path
    ListenAddError path _ -> path
    ListenRemoveStart path _ -> path
    ListenRemoveCancelled path -> path
    ListenRemoveCancelledReset path -> path
    ListenRemoveError path _ -> path
    ReservedFeedback path () -> path


--_-----------------------_--
----- NODE EFFECTOR API -----
--`-----------------------`--


nodeCancel : Context subfeedback -> Config never subtype subfeedback -> Path -> TreeNode subtype -> (TreeNode subtype, List (DB.DBTask never))
nodeCancel =
  nodeCancel_ False


nodeCancelAndReset : Context subfeedback -> Config never subtype subfeedback -> Path -> TreeNode subtype -> (TreeNode subtype, List (DB.DBTask never))
nodeCancelAndReset =
  nodeCancel_ True


nodeCancel_ : Bool -> Context subfeedback -> Config never subtype subfeedback -> Path -> TreeNode subtype -> (TreeNode subtype, List (DB.DBTask never))
nodeCancel_ bCancelAndReset context config path mnode =
  let
    targetPath =
      nodeMetaGet mnode
      |> .targetPath

    targetLocation =
      contextLocation targetPath context

    tagAddError = DB.SubscriptionErrorTag >> ListenAddError path
    tagRemoveError = DB.SubscriptionErrorTag >> ListenRemoveError path

    tagAddCancel = if bCancelAndReset then ListenAddCancelledReset path else ListenAddCancelled path
    tagRemoveCancel = if bCancelAndReset then ListenRemoveCancelledReset path else ListenRemoveCancelled path


    cancelOne tagError tagCancel getter setter listener =
      getter listener
      |> Resource.therefore
        (\subs ->
          ( setter Resource.pending listener
          , [ desubscriptionTask_ context.address tagError tagCancel subs ]
          )
        )
      |> Resource.deriveIf Resource.isPending
        (\_ -> Resource.def (setter Resource.pending listener, []))
      |> Resource.otherwise (listener, [])


    cancelAdd = cancelOne tagAddError tagAddCancel readListenAdd writeListenAdd
    cancelRemove = cancelOne tagRemoveError tagRemoveCancel readListenRemove writeListenRemove
  in
    case mnode of
      Stub meta ->
        (mnode, [])

      Leaf ({depth, size, listener, targetPath, errors} as meta) data ->
        let
          (data', tasks) =
            config.cancellation context path targetPath data

        in
          (Leaf { meta | cancelOnRemove = False } data', tasks)

      Node ({depth, size, listener, targetPath, errors} as meta) ({children, next, delta} as childrenStruct) ->
        let
          ({ childrenStruct', meta' }, tasks) =
            App.chain
              [ App.chain
                  [ App.chainSub .childrenStruct' (\v m -> { m | childrenStruct' = v })
                      [ \m ->
                          Dict.foldl
                            (\key tnode (model', tasks) ->
                              App.chainSub (always tnode) (Just >> childrenSet key)
                                [ App.doEffect (always tasks)
                                , nodeCancel_ bCancelAndReset context config (Path.push key path)
                                ] model'
                            ) (m, []) m.next
                      ]
                  , App.chainSub .meta' (\v m -> { m | meta' = v })
                      [ App.chainSub .listener (\v m -> { m | listener = v })
                          [ cancelAdd
                          , cancelRemove
                          ]
                      , App.asEffector (\m -> { m | cancelOnRemove = False })
                      ]
                  ]
                  |> App.finalizedEffector App.parallel
              ] { meta' = meta, childrenStruct' = childrenStruct }
        in
          (Node meta' childrenStruct', tasks)


nodeSubscribe : Context subfeedback -> Config never subtype subfeedback -> Path -> TreeNode subtype -> (TreeNode subtype, List (DB.DBTask never))
nodeSubscribe context config path =
  nodeSubscribe' (pathContext context path) context config path


subscribers_
  :  Context subfeedback -> ElmFire.OrderOptions -> ElmFire.Location -> Path
  -> { subscribeAdd : Listener -> (Listener, List (DB.DBTask never))
     , subscribeRemove : Listener -> (Listener, List (DB.DBTask never))
     }
subscribers_ context orderOptions targetLocation' path =
  let
    tagAddSub = AddSub
    tagRemoveSub = RemoveSub

    tagAddError = DB.SubscriptionErrorTag >> ListenAddError path
    tagAddStart = ListenAddStart path
    tagAddCancel = ListenAddCancelled path

    tagRemoveError = DB.SubscriptionErrorTag >> ListenRemoveError path
    tagRemoveStart = ListenRemoveStart path
    tagRemoveCancel = ListenRemoveCancelled path

    addQuery = ElmFire.childAdded orderOptions
    removeQuery = ElmFire.childRemoved orderOptions

    onSnapshot tag = fromSnapshot_ context.address tag path
    onCancellation cancelTag errorTag =
      fromCancellation_ context.address cancelTag errorTag

    subscribeOne whichQuery tagError tagStart tagCancel tagSnapshot getter setter listener =
      getter listener
      |> Resource.therefore (\_ -> (listener, []))
      |> Resource.deriveIf Resource.isUnknown
          (\_ ->
            Resource.def
              ( setter Resource.pending listener
              , [ subscriptionTask_ context.address targetLocation'
                    ( tagStart
                    , tagError
                    , onSnapshot tagSnapshot
                    , onCancellation tagCancel tagError
                    ) whichQuery
                ]
              )
          )
      |> Resource.otherwise (listener, [])
  in
    { subscribeAdd =
        subscribeOne addQuery
          tagAddError tagAddStart tagAddCancel tagAddSub
          readListenAdd writeListenAdd
    , subscribeRemove =
        subscribeOne removeQuery
          tagRemoveError tagRemoveStart tagRemoveCancel tagRemoveSub
          readListenRemove writeListenRemove
    }


nodeSubscribe' : Path -> Context subfeedback -> Config never subtype subfeedback -> Path -> TreeNode subtype -> (TreeNode subtype, List (DB.DBTask never))
nodeSubscribe' targetPathInput context config path mnode =
  let
    (targetPath', isData, isManual, orderOptions) =
      case config.policy path of
        Data                                     -> (targetPathInput,  True,  False, ElmFire.noOrder)
        Manual                                   -> (targetPathInput,  False, True,  ElmFire.noOrder)
        Auto orderOptions                        -> (targetPathInput,  False, False, orderOptions)

        DataRebase rebaseTargetPath              -> (rebaseTargetPath, True,  False, ElmFire.noOrder)
        ManualRebase rebaseTargetPath            -> (rebaseTargetPath, False, True,  ElmFire.noOrder)
        AutoRebase rebaseTargetPath orderOptions -> (rebaseTargetPath, False, False, orderOptions)

    targetLocation =
      contextLocation (.targetPath <| nodeMetaGet mnode) context

    targetLocation' =
      contextLocation targetPath' context
  in
    case mnode of
      Stub meta -> -- if we have a stub,
        App.chain
          [ App.chainIf (always isData)
              [ App.asEffector (always <| Leaf { meta | switchController = False } (config.subInit path)) ]
          , App.chainIf (always <| not isData)
              [ App.asEffector (always <| Node { meta | switchController = False } childrenEmpty) ]
          , nodeSubscribe' targetPathInput context config path
          ] mnode

      Leaf ({depth, size, listener, switchController, targetPath, errors} as meta) data ->
        if not isData then
          App.chainIf (nodeMetaGet >> .switchController >> not)
            [ nodeCancel context config path
            , App.chainSub nodeMetaGet nodeMetaSet
                [ App.asEffector (\m -> { m | targetPath = targetPath', switchController = True })
                , App.doEffect (\m -> [Signal.send context.address [ReservedFeedback path ()]])
                ]
            ] mnode
        else if not switchController then
          let
            (data', tasks) =
              config.subscription context path targetPath' data

          in
            (Leaf meta data', tasks)
        else
          (mnode, [])


      Node ({depth, size, listener, targetPath, switchController, errors} as meta) ({children, next, delta} as childrenStruct) ->
        if isData then
          App.chainIf (nodeMetaGet >> .switchController >> not)
            [ nodeCancelAndReset context config path
            , App.chainSub nodeMetaGet nodeMetaSet
                [ App.asEffector (\m -> { m | targetPath = targetPath', switchController = True })
                , App.doEffect (\m -> [Signal.send context.address [ReservedFeedback path ()]])
                ]
            ] mnode
        else if not switchController then
          let
            { subscribeAdd, subscribeRemove } =
              subscribers_ context orderOptions targetLocation' path

            (mnode', tasks') =
              App.chainIf (always <| not (Path.isSame targetPath targetPath'))
                [ nodeCancelAndReset context config path
                , App.chainSub nodeMetaGet nodeMetaSet
                    [ App.asEffector (\m -> { m | targetPath = targetPath' })
                    ]
                ] mnode

            ({ childrenStruct', meta' }, tasks) =
              App.chain
                [ App.chain
                    [ App.chainIf (always <| not isManual)
                        [ App.chainSub .meta' (\v m -> { m | meta' = v })
                            [ App.chainSub .listener (\v m -> { m | listener = v })
                                [ subscribeAdd
                                , subscribeRemove
                                ]
                            ]
                        ]
                    , App.chainSub .childrenStruct' (\v m -> { m | childrenStruct' = v })
                        [ \m ->
                            Dict.foldl
                              (\key (prior, next) (childrenStruct_, tasks) ->
                                let
                                  childTargetPath = Path.push key targetPath'
                                  childPath = Path.push key path

                                  computeCancellations () =
                                    Maybe.map
                                      (\priorChildNode ->
                                        if .cancelOnRemove (nodeMetaGet priorChildNode) then
                                          App.chain
                                            [ App.doEffect (always tasks)
                                            , nodeCancel context config childPath
                                            ] priorChildNode
                                          |> \(cancelledChildNode, tasks') ->
                                              (childrenSet key (Just cancelledChildNode) childrenStruct_, App.finalizeTasks App.parallel tasks')
                                        else
                                          (childrenStruct_, tasks)
                                      ) prior
                                    |> Maybe.withDefault (childrenStruct_, tasks)
                                in
                                  case (prior, next) of
                                    (_, Just childNode) ->
                                      -- we call recursively call nodeSubscribe on any child that has
                                      -- changed in the deltas.
                                      nodeSubscribe' childTargetPath context config childPath childNode
                                      |> \(childNode', tasks') ->
                                          (childrenSet key (Just childNode') childrenStruct_, tasks')

                                    (_, Nothing) ->
                                      -- Here we are reacting to the nodes which need deferred
                                      -- cancellation
                                      computeCancellations ()


                              ) (m, []) m.delta
                        ]
                    ]
                    |> App.finalizedEffector App.parallel
                ] { meta' = nodeMetaGet mnode', childrenStruct' = nodeChildrenGet mnode' |> Maybe.withDefault childrenEmpty }
          in
            (Node meta' childrenStruct', tasks' ++ tasks)
      else
        (mnode, [])


--_----------------------------------_--
----- FINGER UPDATE IMPLEMENTATION -----
--`----------------------------------`--


makeFinger : Path -> TreeNode subtype -> (List (Maybe (TreeNode subtype)))
makeFinger path mnode =
  Path.foldBase
    (\key ls ->
      case ls of
        (Just mnode') :: lsTail ->
          nodeChildrenGetNext key mnode'
          |> flip (::) ls

        Nothing :: lsTail -> Nothing :: ls

        [] -> Debug.crash "Unreachable."
    )
    [Just mnode]
    path


type alias SimpleZipper subtype =
  String -> Path -> TreeNode subtype -> Maybe (TreeNode subtype) -> TreeNode subtype

type alias UpdateZipper subtype =
  String -> Path -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype)

type alias EffectZipper never subtype =
  String -> Path -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> (Maybe (TreeNode subtype), List (DB.DBTask never))


zipFingerSimple : SimpleZipper subtype -> Path -> List (Maybe (TreeNode subtype)) -> TreeNode subtype
zipFingerSimple fzip path =
  zipFingerEffect
    (\key pathParent mMaybeChild mMaybeParent ->
      ( fzip key pathParent
          (Maybe.withDefault (node <| Path.push key pathParent) mMaybeChild)
          mMaybeParent
        |> Just
      , []
      )
    ) path
  >> fst

zipFingerUpdate : UpdateZipper subtype -> Path -> List (Maybe (TreeNode subtype)) -> TreeNode subtype
zipFingerUpdate fzip path =
  zipFingerEffect
    (\key pathParent mMaybeChild mMaybeParent ->
      ( fzip key pathParent mMaybeChild mMaybeParent
      , []
      )
    ) path
  >> fst


zipFingerEffect : EffectZipper never subtype -> Path -> List (Maybe (TreeNode subtype)) -> (TreeNode subtype, List (DB.DBTask never))
zipFingerEffect fzip path finger =
  let
    zipperT (mmaybeNode', tasks) pathStack finger' =
      case (pathStack, finger') of
        (key :: pathStackParent, parent :: fingerParent) ->
          Continue
            (\() -> fzip key (Path.fromStack pathStackParent) mmaybeNode' parent
            |> \(mmaybeNodeParent, tasks') -> zipperT
                (mmaybeNodeParent, (App.finalizeTasks App.parallel tasks) ++ tasks') pathStackParent fingerParent)

        ([], []) -> Done (Maybe.withDefault (node Path.base) mmaybeNode', App.finalizeTasks App.parallel tasks)

        (pathStackBad, fingerBad) ->
          Debug.log
            ("Bad path and finger pair, finger must be (1) element longer "
            ++ "than the path! The root node is present when there is no path. _(path, finger)_")
            (pathStackBad, fingerBad)
          |> \_ -> Debug.crash "Unreachable, finger and path can never differ in length."

    zipper mn ps =
      zipperT (mn, []) ps >> trampoline

  in
    case finger of
      (mmaybeNode :: fingerTail) ->
        -- use the node that was recovered at the location.
        zipper mmaybeNode (Path.toStack path) fingerTail

      _ ->
        Debug.crash "Unreachable; makeFinger will always return a list with at least one element."


fingerZipperSimple : SimpleZipper subtype -> Path -> TreeNode subtype -> TreeNode subtype
fingerZipperSimple fzip path mrootNode =
  makeFinger path mrootNode
  |> zipFingerSimple fzip path


fingerZipperUpdate : UpdateZipper subtype -> Path -> TreeNode subtype -> TreeNode subtype
fingerZipperUpdate fzip path mrootNode =
  makeFinger path mrootNode
  |> zipFingerUpdate fzip path


fingerZipperEffect : EffectZipper never subtype -> Path -> TreeNode subtype -> (TreeNode subtype, List (DB.DBTask never))
fingerZipperEffect fzip path mrootNode =
  makeFinger path mrootNode
  |> zipFingerEffect fzip path


zipRemove
  :  Path
  -> String -> Path -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype)
zipRemove targetPath key pathParent mmaybeChild mmaybeParent =
  case (mmaybeChild, mmaybeParent) of
    (Just mChild, Just mParent) -> -- we found a parent, so we must be on the existing part of the finger.
      if Path.isSame (Path.push key pathParent) targetPath then
        -- we found that we are at the target path for removal. use cancellation set.
        -- this will trigger a recursive cancellation the entire subtree during subscription
        -- processing.
        nodeChildrenSetCancel key mParent
        |> Just
      else
        -- we found that we are past the target path for removal.=
        nodeChildrenSet key (Just mChild) mParent
        |> Just

    (Nothing, Just mParent) ->
      -- if the child argument is a stub, then we got to [2]. Log error and begin rewinding.
      nodeErrorLog (DB.RemovalPathDoesNotExist targetPath) mParent
      |> Just

    (_, Nothing) ->
      -- [2] If we have a Nothing parent argument then the finger for this path is past the
      -- existing structure. There is nothing to remove, so start propagating nothing until the
      -- existing tree is reached, then proceed by cloning a version with the path error.
      Nothing



zipWriteUpdate
  :  Path -> (TreeNode subtype -> TreeNode subtype)
  -> String -> Path -> TreeNode subtype -> Maybe (TreeNode subtype) -> TreeNode subtype
zipWriteUpdate targetPath fupdate key pathParent mChild mmaybeParent =
  let
    child =
      if Path.isSame (Path.push key pathParent) targetPath then fupdate mChild else mChild

  in
    Maybe.withDefault (node pathParent) mmaybeParent
    |> nodeChildrenSet key (Just child)


zipWriteEffect
  :  Path -> (Maybe (TreeNode subtype) -> (Maybe (TreeNode subtype), List (DB.DBTask never)))
  -> String -> Path -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> (Maybe (TreeNode subtype), List (DB.DBTask never))
zipWriteEffect targetPath fupdate key pathParent mmaybeChild mmaybeParent =
  let
    (child, tasks) =
      if Path.isSame (Path.push key pathParent) targetPath then
        fupdate mmaybeChild
      else
        (mmaybeChild, [])

  in
    ( Maybe.withDefault (node pathParent) mmaybeParent
      |> nodeChildrenSet key child
      |> Just
    , tasks
    )


possibleEffector : (modeltype -> (modeltype, List (DB.DBTask never))) -> Maybe modeltype -> (Maybe modeltype, List (DB.DBTask never))
possibleEffector feffector maybeModel =
  Maybe.map feffector maybeModel
  |> Maybe.map (\(m', tasks) -> (Just m', tasks))
  |> Maybe.withDefault (Nothing, [])


zipOverwrite
  :  Path -> TreeNode subtype
  -> String -> Path -> TreeNode subtype -> Maybe (TreeNode subtype) -> TreeNode subtype
zipOverwrite targetPath mNewNode =
  zipWriteUpdate targetPath (always mNewNode)


writeTo : Path -> TreeNode subtype -> TreeNode subtype -> TreeNode subtype
writeTo path mNewNode mnode =
  fingerZipperSimple (zipOverwrite path mNewNode) path mnode


updateTo : Path -> (TreeNode subtype -> TreeNode subtype) -> TreeNode subtype -> TreeNode subtype
updateTo path fupdate mnode =
  fingerZipperSimple (zipWriteUpdate path fupdate) path mnode


effectPath : Path -> (Maybe (TreeNode subtype) -> (Maybe (TreeNode subtype), List (DB.DBTask never))) -> TreeNode subtype -> (TreeNode subtype, List (DB.DBTask never))
effectPath path feffect mnode =
  --String -> Path -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> (Maybe (TreeNode subtype), List (DB.DBTask never))
  fingerZipperEffect (zipWriteEffect path feffect) path mnode


transformFromSub : Path -> (Path -> subfeedback -> subtype -> subtype) -> Config never subtype subfeedback -> subfeedback -> TreeNode subtype -> TreeNode subtype
transformFromSub path subInput config feedback mnode =
  case mnode of
    Stub meta ->
      config.subInit path
      |> Leaf meta
      |> transformFromSub path subInput config feedback -- guaranteed to reach terminating leaf branch below.
    Leaf meta data ->
      subInput path feedback data
      |> Leaf meta
    Node meta childrenStruct ->
      Debug.log "Recieved subfeedback at a group Path!"
      nodeErrorLog (DB.RecievedSubFeedbackForGroup) mnode


--_--------------_--
----- NODE API -----
--`--------------`--


logErrorAt : Path -> DB.DBError -> TreeNode subtype -> TreeNode subtype
logErrorAt path error =
  updateTo path (nodeErrorLog error)


nodePathAdd : Path -> TreeNode subtype -> TreeNode subtype
nodePathAdd path =
  updateTo path identity


nodePathRemove : Path -> TreeNode subtype -> TreeNode subtype
nodePathRemove path =
  fingerZipperUpdate (zipRemove path) path


nodePathSet : Path -> TreeNode subtype -> TreeNode subtype -> TreeNode subtype
nodePathSet =
  writeTo


nodeInputOne : Config never subtype subfeedback -> TreeFeedback subfeedback -> TreeNode subtype -> TreeNode subtype
nodeInputOne config feedback mnode =
  let
    path = feedbackPath feedback

  in
    case (Debug.log "TREE FEEDBACK" feedback) of
      FromSub _ feedback' ->
        updateTo path (transformFromSub path config.subInput config feedback') mnode

      AddSub _ ->
        nodePathAdd path mnode

      RemoveSub _ ->
        nodePathRemove path mnode

      ListenAddStart _ subs ->
        updateTo path (nodeSetAddListener <| Resource.def subs) mnode

      ListenAddCancelled _ ->
        updateTo path
          (nodeSetAddListener (Resource.void)
          >> nodeChildrenClear)
          mnode

      ListenAddCancelledReset _ ->
        updateTo path
          (nodeSetAddListener (Resource.unknown)
          >> nodeChildrenClear)
          mnode

      ListenAddError _ error ->
        logErrorAt path error mnode

      ListenRemoveStart _ subs ->
        updateTo path (nodeSetRemoveListener <| Resource.def subs) mnode

      ListenRemoveCancelled _ ->
        updateTo path
          (nodeSetAddListener (Resource.void)
          >> nodeChildrenClear) mnode

      ListenRemoveCancelledReset _ ->
        updateTo path
          (nodeSetAddListener (Resource.unknown)
          >> nodeChildrenClear) mnode

      ListenRemoveError _ error ->
        logErrorAt path error mnode

      -- Reserved for future use.
      ReservedFeedback _ _ ->
        mnode


nodeStep : TreeNode subtype -> TreeNode subtype
nodeStep mnode =
  case mnode of
    Stub _ -> mnode
    Leaf _ _ -> mnode
    Node meta childrenStruct -> Node meta (childrenStep childrenStruct)


node : Path -> TreeNode subtype
node path =
  Stub { metaDefault | targetPath = path }


nodeSetDepth : Int -> TreeNode subtype -> TreeNode subtype
nodeSetDepth depth' mnode =
  if depth' < 0 then
    Debug.crash "You cannot create a node with a depth less than zero (0)!"
  else
    let meta = nodeMetaGet mnode
    in nodeMetaSet { meta | depth = depth' } mnode


nodeSetSize : Int -> TreeNode subtype -> TreeNode subtype
nodeSetSize size' mnode =
  if size' < 1 then
    Debug.crash "You cannot create a node with a size less than one (1)!"
  else
    let meta = nodeMetaGet mnode
    in nodeMetaSet { meta | size = size' } mnode


nodeSetListener : Listener -> TreeNode subtype -> TreeNode subtype
nodeSetListener listener mnode =
  let meta = nodeMetaGet mnode
  in nodeMetaSet { meta | listener = listener } mnode


nodeSetAddListener : Resource DB.DBError ElmFire.Subscription -> TreeNode subtype -> TreeNode subtype
nodeSetAddListener subs mnode =
  nodeMetaGet mnode
  |> \meta -> meta.listener
  |> writeListenAdd subs
  |> \listener' -> nodeMetaSet { meta | listener = listener' } mnode


nodeSetRemoveListener : Resource DB.DBError ElmFire.Subscription -> TreeNode subtype -> TreeNode subtype
nodeSetRemoveListener subs mnode =
  nodeMetaGet mnode
  |> \meta -> meta.listener
  |> writeListenRemove subs
  |> \listener' -> nodeMetaSet { meta | listener = listener' } mnode


nodeSetTargetPath : Path -> TreeNode subtype -> TreeNode subtype
nodeSetTargetPath targetPath mnode =
  let meta = nodeMetaGet mnode
  in nodeMetaSet { meta | targetPath = targetPath } mnode


nodeErrorLog : DB.DBError -> TreeNode subtype -> TreeNode subtype
nodeErrorLog error mnode =
  let meta = nodeMetaGet mnode
  in nodeMetaSet { meta | errors = error :: meta.errors } mnode


nodeSetSwitchController : Bool -> TreeNode subtype -> TreeNode subtype
nodeSetSwitchController bSwitchController mnode =
  let meta = nodeMetaGet mnode
  in nodeMetaSet { meta | switchController = bSwitchController } mnode


nodeSetCancelOnRemove : Bool -> TreeNode subtype -> TreeNode subtype
nodeSetCancelOnRemove bCancelOnRemove mnode =
  let meta = nodeMetaGet mnode
  in nodeMetaSet { meta | cancelOnRemove = bCancelOnRemove } mnode


nodeMetaGet : TreeNode subtype -> NodeMeta
nodeMetaGet mnode =
  case mnode of
    Stub meta -> meta
    Leaf meta _ -> meta
    Node meta _ -> meta


nodeMetaSet : NodeMeta -> TreeNode subtype -> TreeNode subtype
nodeMetaSet meta mnode =
  case mnode of
    Stub _ -> Stub meta
    Leaf _ data -> Leaf meta data
    Node _ childrenStruct -> Node meta childrenStruct


nodeChildrenReplace : Children subtype -> TreeNode subtype -> TreeNode subtype
nodeChildrenReplace childrenStruct mnode =
  case mnode of
    Stub meta -> Node meta childrenStruct
    Node meta _ -> Node meta childrenStruct
    Leaf _ _ -> Debug.log "Cannot replace children in leaf node" mnode


nodeChildrenSetCancel : String -> TreeNode subtype -> TreeNode subtype
nodeChildrenSetCancel key =
  nodeChildrenSet' True key Nothing


nodeChildrenSet : String -> Maybe (TreeNode subtype) -> TreeNode subtype -> TreeNode subtype
nodeChildrenSet =
  nodeChildrenSet' False


nodeChildrenClearCancel : TreeNode subtype -> TreeNode subtype
nodeChildrenClearCancel mnode =
  case mnode of
    Stub _ -> mnode
    Node meta childrenStruct ->
      childrenClearCancel childrenStruct
      |> Node meta
    Leaf _ _ -> Debug.log "Cannot replace children in leaf node" mnode


nodeChildrenClear : TreeNode subtype -> TreeNode subtype
nodeChildrenClear mnode =
  case mnode of
    Stub _ -> mnode
    Node meta childrenStruct ->
      childrenClear childrenStruct
      |> Node meta
    Leaf _ _ -> Debug.log "Cannot replace children in leaf node" mnode


nodeChildrenSet' : Bool -> String -> Maybe (TreeNode subtype) -> TreeNode subtype -> TreeNode subtype
nodeChildrenSet' bCancelAndReset key mnode' mnode =
  case mnode of
    Stub meta ->
      if mnode' /= Nothing then
        childrenEmpty
        |> childrenSet' bCancelAndReset key mnode'
        |> Node meta
        |> nodeSetCancelOnRemove bCancelAndReset
      else
        mnode

    Leaf meta _ ->
      Debug.log "Attempted nodeChildrenSet on a Leaf node." mnode
      |> \_ -> nodeErrorLog (DB.WriteLocationParentIsData) mnode

    Node meta childrenStruct ->
      childrenStruct
      |> childrenSet' bCancelAndReset key mnode'
      |> Node meta
      |> nodeSetCancelOnRemove bCancelAndReset


nodeChildrenGet : TreeNode subtype -> Maybe (Children subtype)
nodeChildrenGet mnode =
  case mnode of
    Stub _ -> Nothing
    Leaf _ _ -> Nothing
    Node _ childrenStruct -> Just childrenStruct

nodeChildrenGetCurrent : String -> TreeNode subtype -> Maybe (TreeNode subtype)
nodeChildrenGetCurrent key =
  nodeChildrenGet
  >> Maybe.map (childrenGetCurrent key)
  >> Maybe.withDefault Nothing

nodeChildrenGetNext : String -> TreeNode subtype -> Maybe (TreeNode subtype)
nodeChildrenGetNext key =
  nodeChildrenGet
  >> Maybe.map (childrenGetNext key)
  >> Maybe.withDefault Nothing

nodeChildrenGetDelta : String -> TreeNode subtype -> (Maybe (TreeNode subtype), Maybe (TreeNode subtype))
nodeChildrenGetDelta key =
  nodeChildrenGet
  >> Maybe.map (childrenGetDelta key)
  >> Maybe.withDefault (Nothing, Nothing)

nodeChildrenFoldDeltaLeft : (String -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> outtype -> outtype) -> outtype -> TreeNode subtype -> outtype
nodeChildrenFoldDeltaLeft f out =
  nodeChildrenGet
  >> Maybe.map (childrenFoldDeltaLeft f out)
  >> Maybe.withDefault out

nodeChildrenFoldDeltaRight : (String -> Maybe (TreeNode subtype) -> Maybe (TreeNode subtype) -> outtype -> outtype) -> outtype -> TreeNode subtype -> outtype
nodeChildrenFoldDeltaRight f out =
  nodeChildrenGet
  >> Maybe.map (childrenFoldDeltaRight f out)
  >> Maybe.withDefault out


-- INTERNAL
fromSnapshot_ : Signal.Address (List action) -> (Path -> action) -> Path -> ElmFire.Snapshot -> Task never ()
fromSnapshot_ address tag path snapshot =
  Signal.send address [tag <| Path.push snapshot.key path]

fromCancellation_ : Signal.Address (List action) -> action -> (ElmFire.Error -> action) -> ElmFire.Cancellation -> Task never ()
fromCancellation_ address cancelTag subscriptionTag cancl =
  case cancl of
    ElmFire.Unsubscribed _ -> Signal.send address [cancelTag]
    ElmFire.QueryError _ error -> Signal.send address [subscriptionTag error]

subscriptionTask_
  : Signal.Address (List action)
  -> ElmFire.Location
  ->  ( ElmFire.Subscription -> action
      , ElmFire.Error -> action
      , ElmFire.Snapshot -> Task never ()
      , ElmFire.Cancellation -> Task never ()
      )
  -> ElmFire.Query
  -> Task never ()
subscriptionTask_ address location (onSubs, onErr, onSnapshot, onCancellation) query' =
  (ElmFire.subscribe onSnapshot onCancellation query' location
    `andThen` (\subs -> Signal.send address [onSubs subs])
    `onError` (\error -> Signal.send address [onErr error]))

desubscriptionTask_
  : Signal.Address (List action)
  -> (ElmFire.Error -> action)
  -> action
  -> ElmFire.Subscription
  -> Task never ()
desubscriptionTask_ address onErr onCancel subs =
  ElmFire.unsubscribe subs
    `andThen` (\_ -> Signal.send address [onCancel])
    `onError` (\error -> Signal.send address [onErr error])



--_-------------------------_--
----- TREE API DEFINITION -----
--`-------------------------`--


{-| A new empty free. -}
newTree : Tree subtype
newTree =
  Tree
    { root = node Path.base
    }


{-| Cancel at a particular path. -}
cancel : Context subfeedback -> Config never subtype subfeedback -> Path -> Tree subtype -> (Tree subtype, List (DB.DBTask never))
cancel context config path tree =
  App.chainSub (\(Tree v) -> v) (\v -> always (Tree v))
    [ App.chainSub .root (\v m -> { m | root = v })
        [ effectPath path (possibleEffector <| nodeCancel context config path)
        ]
        |> App.finalizedEffector App.parallel
    ] tree


{-| Cancel and reset a particular path. -}
cancelAndReset : Context subfeedback -> Config never subtype subfeedback -> Path -> Tree subtype -> (Tree subtype, List (DB.DBTask never))
cancelAndReset context config path tree =
  App.chainSub (\(Tree v) -> v) (\v -> always (Tree v))
    [ App.chainSub .root (\v m -> { m | root = v })
        [ effectPath path (possibleEffector <| nodeCancelAndReset context config path)
        ]
        |> App.finalizedEffector App.parallel
    ] tree


{-| -}
subscribe : Context subfeedback -> Config never subtype subfeedback -> Tree subtype -> (Tree subtype, List (DB.DBTask never))
subscribe context config tree =
  App.chainSub (\(Tree v) -> v) (\v -> always (Tree v))
    [ App.chainSub .root (\v m -> { m | root = v })
        [ nodeSubscribe context config Path.base
        ]
        |> App.finalizedEffector App.parallel
    ] tree


{-| -}
addPath : Path -> Tree subtype -> Tree subtype
addPath path (Tree struct) =
  Tree
    { struct
    | root = nodePathAdd path struct.root
    }


{-| -}
removePath : Path -> Tree subtype -> Tree subtype
removePath path (Tree struct) =
  Tree
    { struct
    | root = nodePathRemove path struct.root
    }


{-| -}
inputOne : Config never subtype subfeedback -> TreeFeedback subfeedback -> Tree subtype -> Tree subtype
inputOne config feedback (Tree struct) =
  Tree
    { struct
    | root = nodeInputOne config feedback struct.root
    }


{-| -}
step : Tree subtype -> Tree subtype
step (Tree struct) =
  Tree
    { struct
    | root = nodeStep struct.root
    }
