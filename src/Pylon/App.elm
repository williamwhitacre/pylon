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


module Pylon.App

  ( Options
  , Config
  , Output

  , Dispatch
  , ActionTask
  , FinalTask

  , parallel, sequence

  , defaultOptions
  , optionsDispatchMethod
  , optionsRunInputsOnStartup

  , nilConfig
  , configWithOptions

  , chain
  , chainIf
  , chainWhile
  , chainSub
  , chainSubIf
  , chainFinalizingEach
  , finalizedEffector
  , asEffector
  , mappedEffector
  , noEffect
  , doEffect

  , configOptions, configInit
  , configUpdateList, configUpdate
  , configStage, configPresent
  , configInput
  , configListInput
  , configRawInput

  , promoteActions
  , handleErrors
  , mapErrors
  , sendActions
  , forwardActions
  , sendErrorActions
  , sendResultActions
  , finalizeTasks
  , finalizeTask
  , thenDo
  , thenDoNothing

  , doCycle, run

  , doSubUpdate, doSubStage, doSubPresent, doSubCycle

  ) where



{-| Application module. Similar to StartApp, but with a few extra utilities. Heavily pruned and
simplified with respect to it's predecessor in Scaffold. No longer suffers from heinous type
bloat.


# Configuration And Application Structures

The basic building blocks of the application top level.

@docs Options, Config, Output


# Task Types

@docs Dispatch, ActionTask, FinalTask


# Task List Dispatch Methods

@docs parallel, sequence


# Constructing Options

@docs defaultOptions, optionsDispatchMethod, optionsRunInputsOnStartup


# Configuration DSL

Cumulatively build a configuration using this convenient DSL, starting either from a `nilConfig`
or starting from a `configWithOptions`. The latter can be very nice for flowing right in to building
a configuration from building an `Options` struct.

@docs nilConfig, configWithOptions, configOptions, configInit, configUpdateList, configUpdate, configStage, configPresent, configInput, configListInput, configRawInput

# Chaining Effectors

Effectors are functions which give a pair, the first element of which is the of the same type as the
last argument, and the second element of which is some list of tasks. Chaining is a useful way to
operate on a single model with a sequence of actions, all of which might produce tasks. We use
lists of tasks so that we can elegantly account for the case where no tasks are produced as well as
possibly producing a number of tasks to execute. Such task lists can be either explicitly reduced
by the user using `finalizeTasks`, or they can be converted to `ActionTask`s and returned to the
top level, where the configured `Dispatch` method is used to run the tasks.

@docs chain, chainIf, chainWhile, chainSub, chainSubIf, chainFinalizingEach, asEffector, mappedEffector, noEffect, doEffect, finalizedEffector


# Task Manipulation

Send the results of tasks to addresses, and dispatch them. Use of existing task primitives to handle
situations outside the scope of simply sending/forwarding results to the app's mailbox address and
determining their order of execution is encouraged, so that a repeat of the awful `agent` DSL
from Pylon's predecessor Scaffold is not repeated.

@docs promoteActions, handleErrors, mapErrors, sendActions, forwardActions, sendErrorActions, sendResultActions, finalizeTasks, finalizeTask, thenDo, thenDoNothing


# Running The Application

`run` will allow you to run an application given an initial model and view with a configuration.
`doCycle` exposes the internal functionality used by run, with a little syntactic sugar.

@docs doCycle, run


# Manipulating Sub Models
@docs doSubUpdate, doSubStage, doSubPresent, doSubCycle

-}

import Signal exposing (Signal)
import Signal.Extra as SignalEx

import Task exposing (Task, andThen, onError)
import Task.Extra as TaskEx

import Time exposing (Time)

import Trampoline exposing (..)


{-| Task dispatch mode. -}
type Dispatch =
  Parallel
  | Sequence


{-| Dispatch tasks in parallel, no ordering is required. -}
parallel : Dispatch
parallel = Parallel


{-| Dispatch tasks in sequence. Tasks will execute strictly in the order provided. -}
sequence : Dispatch
sequence = Sequence


{-| A task which results in a list of actions. This can be routed to an address. -}
type alias ActionTask errortype actiontype = Task errortype (List actiontype)


{-| FinalTask represents a task whose results have been routed to an address, and is ready to be
executed. -}
type alias FinalTask never = Task never ()


{-| Some extra options for apps.

    runInputsOnStartup    -- If this is true, run the actions produced by the initial input signals
                          -- on startup.

    dispatchMethod        -- May either dispatch the top level tasks returned in sequence or in
                          -- parallel.

-}
type alias Options =
  { runInputsOnStartup : Bool
  , dispatchMethod : Dispatch
  }


{-| Configure an application. Sane defaults are provided so that you can construct as much or as
little as you need using the provided DSL. -}
type alias Config never modeltype actiontype viewtype =
  { init : Time -> modeltype -> (modeltype, List (FinalTask never))
  , inputs : List (Signal (List actiontype))
  , update : List actiontype -> Time -> modeltype -> (modeltype, List (FinalTask never))
  , stage : Signal.Address (List actiontype) -> Time -> modeltype -> (modeltype, List (FinalTask never))
  , present : Signal.Address (List actiontype) -> Time -> modeltype -> (Maybe viewtype, List (FinalTask never))
  , options : Options
  }


{-| Output of the application. Contains the current view, the current model, the current time, and the latest task if it exists. -}
type alias Output never modeltype viewtype =
  { view : Signal viewtype
  , model : Signal modeltype
  , now : Signal Time
  , tasks : Signal (FinalTask never)
  }



{-| Default options. `runInputsOnStartup` set to `True`. -}
defaultOptions : Options
defaultOptions =
  { runInputsOnStartup = True
  , dispatchMethod = Sequence
  }


{-| Set the dispatch method for the top level ActionTasks returned by application functions. -}
optionsDispatchMethod : Dispatch -> Options -> Options
optionsDispatchMethod mode options = { options | dispatchMethod = mode }


{-| Set whether or not to run options. -}
optionsRunInputsOnStartup : Bool -> Options -> Options
optionsRunInputsOnStartup runInputsOnStartup options =
  { options | runInputsOnStartup = runInputsOnStartup }



{-| Empty configuration, providing all defaults and nil behavior. -}
nilConfig : Config never modeltype actiontype viewtype
nilConfig =
  { init = (\now model -> (model, []))
  , inputs = []
  , update = (\actions now model -> (model, []))
  , stage = (\address now model -> (model, []))
  , present = (\address now model -> (Nothing, []))
  , options = defaultOptions
  }


{-| A nil configuration, with the given options set. -}
configWithOptions : Options -> Config never modeltype actiontype viewtype
configWithOptions options =
  configOptions options nilConfig


{-| Utility function for chaining effectors. -}
chain : List (modeltype -> (modeltype, List (Task z r))) -> modeltype -> (modeltype, List (Task z r))
chain functions data =
  chain_ functions data
  |> \(data', taskLists) -> (data', List.concat taskLists)


{-| Conditionally chain some effectors. -}
chainIf : (modeltype -> Bool) -> List (modeltype -> (modeltype, List (Task z r))) -> modeltype -> (modeltype, List (Task z r))
chainIf predicate functions data =
  if predicate data then
    chain functions data
  else
    (data, [])


{-| Loop a list of effectors until the predicate is no longer satisfied. -}
chainWhile : (modeltype -> Bool) -> List (modeltype -> (modeltype, List (Task z r))) -> modeltype -> (modeltype, List (Task z r))
chainWhile predicate functions data =
  trampoline (chainWhile' (data, []) predicate functions)
  |> \(data', taskLists) -> (data', List.foldr (++) [] taskLists)


chainWhile' : (modeltype, List (List (Task z r))) -> (modeltype -> Bool) -> List (modeltype -> (modeltype, List (Task z r))) -> Trampoline (modeltype, List (List (Task z r)))
chainWhile' (data, taskLists) predicate functions =
  if predicate data then
    chain functions data
    |> \(data', tasks) ->
        Continue
          (\() -> chainWhile' (data', tasks :: taskLists) predicate functions)
  else
    Done (data, taskLists)


{-| Chain a list of effectors on a sub model. Provide a fetch and an update function to extract
and replace the sub model, before and after executing the chain of effectors on the sub model
respectively. -}
chainSub
  :  (modeltype -> innertype)
  -> (innertype -> modeltype -> modeltype)
  -> List (innertype -> (innertype, List (Task z r)))
  -> modeltype
  -> (modeltype, List (Task z r))
chainSub fget fupdate functions data =
  chain functions (fget data)
  |> \(sub', tasks') -> (fupdate sub' data, tasks')


{-| Conditionally subchain some effectors. -}
chainSubIf
  :  (modeltype -> Bool)
  -> (modeltype -> innertype)
  -> (innertype -> modeltype -> modeltype)
  -> List (innertype -> (innertype, List (Task z r)))
  -> modeltype
  -> (modeltype, List (Task z r))
chainSubIf predicate fget fupdate functions data =
  if predicate data then
    chainSub fget fupdate functions data
  else
    (data, [])


{-| Chaining effectors, finalizing each resultant task list seperately using the given dispatch
method. -}
chainFinalizingEach : Dispatch -> List (modeltype -> (modeltype, List (Task z r))) -> modeltype -> (modeltype, List (FinalTask never))
chainFinalizingEach mode functions data =
  chain_ functions data
  |> \(data', taskLists) -> taskLists
  |> List.concatMap (finalizeTasks mode)
  |> \dispatched -> (data', dispatched)


chain_ : List (modeltype -> (modeltype, List (Task z r))) -> modeltype -> (modeltype, List (List (Task z r)))
chain_ functions data =
  case functions of
    [] -> (data, [])
    f :: fs' ->
      f data
      |> \(data', tasks') -> chain_ fs' data'
      |> \(data_, taskLists) -> (data_, tasks' :: taskLists)


{-| Transform the task output of an effector. This is useful if an effector works on the right type of model
but needs a shim to fit the correct task type -}
mappedEffector : (List (Task z r) -> List (Task z' r')) -> (modeltype -> (modeltype, List (Task z r))) -> modeltype -> (modeltype, List (Task z' r'))
mappedEffector mapper effector data =
  effector data
  |> \(data', tasks) -> mapper tasks
  |> \tasks' -> (data', tasks')


{-| Transform an effector by finalizing it's resulting tasks immediately using the given dispatch method. -}
finalizedEffector : Dispatch -> (modeltype -> (modeltype, List (Task z r))) -> modeltype -> (modeltype, List (FinalTask never))
finalizedEffector mode effector data =
  mappedEffector (finalizeTasks mode) effector data


{-| If you want to take a simple transformation `a -> a` and include it in a chain of
`a -> (a, List (Task z r))`, this is your tool for doing that. -}
asEffector : (modeltype -> modeltype) -> modeltype -> (modeltype, List (Task z r))
asEffector func data =
  (func data, [])


{-| This gives you a simple way of emitting effects in a chain without changing the model. -}
doEffect : (modeltype -> List (Task z r)) -> modeltype -> (modeltype, List (Task z r))
doEffect feffect data =
  (data, feffect data)


{-| An effector that does nothing and produces no tasks. Useful as a placeholder when no action
is required. -}
noEffect : modeltype -> (modeltype, List (Task z r))
noEffect = asEffector identity


{-| Add an update function which is aware of an entire atomic action list as it comes in. -}
configUpdateList
  :  (List actiontype -> Time -> modeltype -> (modeltype, List (FinalTask never)))
  -> Config never modeltype actiontype viewtype
  -> Config never modeltype actiontype viewtype
configUpdateList fupdatelist config =
  { config | update = fupdatelist }


{-| Add a classic single action update function, which is simpler and just as good for most use
cases. -}
configUpdate
  :  (actiontype -> Time -> modeltype -> (modeltype, List (FinalTask never)))
  -> Config never modeltype actiontype viewtype
  -> Config never modeltype actiontype viewtype
configUpdate fupdateone =
  let
    updateEach actionList now model =
      List.foldl
        (\action (m', t') ->
          fupdateone action now m'
          |> \(m'', t'') -> (m'', t' ++ t'')
        )
        (model, [])
        actionList

  in
    configUpdateList updateEach


{-| Set some extra options on the configuration. -}
configOptions : Options -> Config never modeltype actiontype viewtype -> Config never modeltype actiontype viewtype
configOptions options config = { config | options = options }


{-| Set an initialization function on the app. -}
configInit
  :  (Time -> modeltype -> (modeltype, List (FinalTask never)))
  -> Config never modeltype actiontype viewtype
  -> Config never modeltype actiontype viewtype
configInit finit config =
  { config
  | init = finit
  }


{-| Configure an input. -}
configInput
  :  (inputtype -> actiontype)
  -> Signal inputtype
  -> Config never modeltype actiontype viewtype
  -> Config never modeltype actiontype viewtype
configInput faction =
  Signal.map (faction >> flip (::) []) >> configRawInput


{-| Configure a list input. -}
configListInput
  :  (List inputtype -> List actiontype)
  -> Signal (List inputtype)
  -> Config never modeltype actiontype viewtype
  -> Config never modeltype actiontype viewtype
configListInput factions =
  Signal.map factions >> configRawInput


{-| Configure a raw input. -}
configRawInput
  :  Signal (List actiontype)
  -> Config never modeltype actiontype viewtype
  -> Config never modeltype actiontype viewtype
configRawInput inputSignal config =
  { config
  | inputs = inputSignal :: config.inputs
  }


{-| Set a staging function on the app. Staging is done after all actions are passed through update,
but before presentation. -}
configStage
  :  (Signal.Address (List actiontype) -> Time -> modeltype -> (modeltype, List (FinalTask never)))
  -> Config never modeltype actiontype viewtype
  -> Config never modeltype actiontype viewtype
configStage fstage config =
  { config
  | stage = fstage
  }


{-| Set a presentation function on the app. This can optionally update the view of the application,
or not by returning nothing. -}
configPresent
  :  (Signal.Address (List actiontype) -> Time -> modeltype -> (Maybe viewtype, List (FinalTask never)))
  -> Config never modeltype actiontype viewtype
  -> Config never modeltype actiontype viewtype
configPresent fpresent config =
  { config
  | present = fpresent
  }


{-| Send the list of actions produced on the success of this task to the given address.  -}
sendActions : Signal.Address (List actiontype) -> ActionTask errortype actiontype -> ActionTask errortype actiontype
sendActions address task =
  TaskEx.interceptSuccess address task


{-| Transform the action list type of an `ActionTask`. -}
promoteActions : (List actiontype -> List actiontype') -> ActionTask errortype actiontype -> ActionTask errortype actiontype'
promoteActions transform task =
  task `andThen` (transform >> Task.succeed)


{-| Transform errors from the given task in to actions. -}
handleErrors : (errortype -> List actiontype) -> ActionTask errortype actiontype -> ActionTask z actiontype
handleErrors promote task =
  task `onError` (promote >> Task.succeed)


{-| Map the errors of a given task to another type of error. -}
mapErrors : (errortype -> errortype') -> Task errortype r -> Task errortype' r
mapErrors transform task =
  task `onError` (transform >> Task.fail)


{-| Forward the list of actions produced on the success of this task to the given address using the given transformation function. -}
forwardActions : (List actiontype -> List actiontype') -> Signal.Address (List actiontype') -> ActionTask errortype actiontype -> ActionTask errortype actiontype'
forwardActions transform address task =
  promoteActions transform task
  |> TaskEx.interceptSuccess address


{-| Translate task error to a list of actions and send to the given address. -}
sendErrorActions : (errortype -> List actiontype) -> Signal.Address (List actiontype) -> ActionTask errortype actiontype -> ActionTask errortype actiontype
sendErrorActions errorHandler address task =
  TaskEx.interceptError (Signal.forwardTo address errorHandler) task


{-| Interpret the result of the task whether success or failure as some list of actions and then send. -}
sendResultActions : (Result errortype successtype -> List actiontype) -> Signal.Address (List actiontype) -> Task errortype successtype -> ActionTask errortype actiontype
sendResultActions resultHandler address task =
  Task.toResult task
  |> Task.map resultHandler
  |> TaskEx.interceptSuccess address


maybeDispatchTask : Dispatch -> List (Task e r) -> Maybe (FinalTask never)
maybeDispatchTask mode tasks =
  case tasks of
    [] -> Nothing
    _ ->
      Just (case mode of
        Parallel ->
          TaskEx.parallel tasks
            `andThen` (\_ -> Task.succeed ())
            `onError` (\_ -> Task.succeed ())

        Sequence ->
          TaskEx.optional tasks
            `andThen` (\_ -> Task.succeed ())
            `onError` (\_ -> Task.succeed ()))


{-| Dispatch any arbitrary list of tasks using the given dispatch mode, which may be `parallel` or
`sequence`. Note that for the sake of cleaner typing, the output of this function is a list of
`FinalTask`s, however, this list may only have one member in the case that tasks are being
dispatched, otherwise empty if the input list was empty. This is because finalizing a list of
tasks will collapse it, encoding the manner in which is should execute in the resultant task if
it exists. -}
finalizeTasks : Dispatch -> List (Task e r) -> List (FinalTask never)
finalizeTasks mode tasks =
  maybeDispatchTask mode tasks
  |> Maybe.map (\task -> [task])
  |> Maybe.withDefault []


{-| Convert any task in to a `FinalTask`. This does not take a `Dispatch` method, because said
method only makes sense when applied to a list of tasks, not a single task. -}
finalizeTask : Task e r -> List (FinalTask never)
finalizeTask = flip (::) [] >> finalizeTasks Sequence


{-| Turn any task in to an `ActionTask` by performing the given list of actions upon it's successful
completion. Notably, this may be used to turn an `FinalTask` back in to an action task. -}
thenDo : List actiontype -> Task errortype r -> ActionTask errortype actiontype
thenDo actions task =
  task `andThen` (\_ -> Task.succeed actions)


{-| Turn any task in to an `ActionTask` without actually performing any actions. This is good for
transforming tasks that need to be returned from the top level of the application which have already
been finalized, so the types simply have to match. It should be noted that empty action lists do
not trigger any update, staging, or presentation, and as such a busy loop caused by repeated
dispatchment of empty action tasks will not occur. -}
thenDoNothing : Task errortype r -> ActionTask errortype a
thenDoNothing = thenDo []


{-| Perform a full cycle on a `(model, view)` pair, using the given application configuration. -}
doCycle
  :  Config never modeltype actiontype viewtype
  -> Signal.Address (List actiontype)
  -> List actiontype
  -> Time -> (modeltype, viewtype)
  -> ((modeltype, viewtype), List (FinalTask never))
doCycle config address actions now (model, view) =
  let
    ((model', maybeView'), tasks) =
      doCycle_ config address actions now (model, view)

  in
    ((model', Maybe.withDefault view maybeView'), tasks)



{-| Perform a full cycle with action promotion. Useful for nesting models. This is
deliberately left very unambiguous to avoid obfuscation. -}
doSubCycle
  :  Config never modeltype actiontype viewtype
  -> (List actiontype -> List outeraction)
  -> Signal.Address (List outeraction)
  -> List actiontype
  -> Time -> (modeltype, viewtype)
  -> ((modeltype, viewtype), List (FinalTask never))
doSubCycle config factionup address actions now inputs =
  doCycle config (Signal.forwardTo address factionup) actions now inputs
  |> \(result, subtasks') -> subtasks'
  |> finalizeTasks sequence
  |> (,) result


{-| Update a sub model. -}
doSubUpdate
  :  Config never modeltype actiontype viewtype
  -> (List actiontype -> List outeraction)
  -> List actiontype
  -> Time -> modeltype
  -> (modeltype, List (FinalTask never))
doSubUpdate config factionup actions now =
  config.update actions now
  |> mappedEffector (finalizeTasks sequence)


{-| Stage a sub model. -}
doSubStage
  :  Config never modeltype actiontype viewtype
  -> (List actiontype -> List outeraction)
  -> Signal.Address (List outeraction)
  -> Time -> modeltype
  -> (modeltype, List (FinalTask never))
doSubStage config factionup address now =
  config.stage (Signal.forwardTo address factionup) now
  |> mappedEffector (finalizeTasks sequence)


{-| Present a sub model. -}
doSubPresent
  :  Config never modeltype actiontype viewtype
  -> (List actiontype -> List outeraction)
  -> Signal.Address (List outeraction)
  -> Time -> modeltype
  -> (Maybe viewtype, List (FinalTask never))
doSubPresent config factionup address now model =
  config.present (Signal.forwardTo address factionup) now model
  |> \(maybeView, subtasks') -> subtasks'
  |> finalizeTasks sequence
  |> (,) maybeView


doCycle_
  :  Config never modeltype actiontype viewtype
  -> Signal.Address (List actiontype)
  -> List actiontype
  -> Time -> (modeltype, viewtype)
  -> ((modeltype, Maybe viewtype), List (FinalTask never))
doCycle_ config address actions now (model, view) =
  let
    (model', updateTasks) =
      chain
        [ config.update actions now
        , config.stage address now
        ] model

    (maybeView', viewTasks) =
      config.present address now model'
  in
    ((model', maybeView'), updateTasks ++ viewTasks)


{-| With the given configuration, start the application as in StartApp with the given initial model,
and the given default view. -}
run : Config never modeltype actiontype viewtype -> modeltype -> viewtype -> Output never modeltype viewtype
run config model0 view0 =
  let
    { init, inputs, update, stage, present, options } =
      config

    { runInputsOnStartup, dispatchMethod } =
      options


    appMailbox =
      Signal.mailbox []


    inputSignal =
      List.foldl (SignalEx.fairMerge (++)) appMailbox.signal inputs
      |> Signal.filterMap
          (\actions ->
            case actions of
              [] -> Nothing
              _ -> Just actions)
          []
      |> Time.timestamp


    doCycleFold_ (now, actions) ((model, view), _) =
      doCycle_ config appMailbox.address actions now (model, Maybe.withDefault view0 view)


    initFunction (now, actions) =
      init now model0
      |> \(model0', initTasks) ->
          doCycle_ config appMailbox.address
            (if runInputsOnStartup then actions else [])
            now (model0', view0)
      |> \((model0'', view0''), cycleTasks) ->
          ((model0'', view0''), initTasks ++ cycleTasks)

    outputSignal =
      SignalEx.foldp'
        doCycleFold_
        initFunction
        inputSignal

  in
    { view = Signal.filterMap (fst >> snd) view0 outputSignal
    , model = Signal.map (fst >> fst) outputSignal
    , tasks = Signal.filterMap (snd >> maybeDispatchTask dispatchMethod) (Task.succeed ()) outputSignal
    , now = Signal.map fst inputSignal
    }
