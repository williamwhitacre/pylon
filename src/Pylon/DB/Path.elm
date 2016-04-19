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


module Pylon.DB.Path
  ( Path

  , base
  , single

  , top
  , last
  , first
  , depth
  , countElements

  , push
  , child
  , pop
  , parent
  , up
  , truncate

  , toStack
  , toList
  , fromStack
  , fromList

  , foldBase
  , foldTop
  , join

  , childPath
  , childLocation
  , absoluteLocation

  , isSame
  ) where

{-| This path module is designed to work with Pylon.DB.Tree and ElmFire.

Paths must be manipulated using this API. You can transform ElmFire locations with paths.

# Path Type
@docs Path

# Constructors
@docs base, single

# List Conversion
@docs toList, fromList, toStack, fromStack

# Inquiry
@docs countElements, top, last, first, depth

# Walking
@docs child, parent, push, pop, up, truncate

# Special Operations
@docs foldBase, foldTop, join

# Manipulations for `ElmFire.Location`
@docs childPath, childLocation, absoluteLocation

# Comparisons
@docs isSame

-}


import Trampoline exposing (..)

import ElmFire
import List


type alias PathStruct_ =
  { stack : List String
  , count : Int
  }

{-| Opaque type representing a path. -}
type Path =
  Base | Path PathStruct_


pathStructEmpty : PathStruct_
pathStructEmpty =
  { stack = []
  , count = 0
  }


pathStructOne : String -> PathStruct_
pathStructOne singleton =
  { stack = [singleton]
  , count = 1
  }


pathStructFromList : List String -> PathStruct_
pathStructFromList ls =
  List.foldl pathStructPush pathStructEmpty ls


pathStructFromStack : List String -> PathStruct_
pathStructFromStack stack =
  { stack = stack
  , count = List.length stack
  }


pathStructCount : PathStruct_ -> Int
pathStructCount = .count


pathStructPush : String -> PathStruct_ -> PathStruct_
pathStructPush element struct =
  { stack = element :: struct.stack
  , count = struct.count + 1
  }


pathStructPop : PathStruct_ -> PathStruct_
pathStructPop struct =
  case struct.stack of
    [] -> struct
    _ :: ltail ->
      { stack = ltail
      , count = struct.count - 1
      }


pathStructTop : PathStruct_ -> Maybe String
pathStructTop =
  .stack >> List.head


pathStructList : PathStruct_ -> List String
pathStructList =
  .stack >> List.reverse


pathStructJoin : String -> PathStruct_ -> String
pathStructJoin str =
  pathStructFoldTop
    (\element ->
      Maybe.map ((++) (element ++ str))
      >> Maybe.withDefault element
      >> Just
    ) Nothing
  >> Maybe.withDefault ""


pathStructFoldBase : (String -> typeout -> typeout) -> typeout -> PathStruct_ -> typeout
pathStructFoldBase f inp struct =
  List.foldr f inp struct.stack


pathStructFoldTop : (String -> typeout -> typeout) -> typeout -> PathStruct_ -> typeout
pathStructFoldTop f inp struct =
  List.foldl f inp struct.stack



--_--------_--
-- PATH API --
--`--------`--


{-| A base path. Represents no path. -}
base : Path
base = Base


{-| A path with a single element. -}
single : String -> Path
single =
  pathStructOne >> Path


{-| Count of the elements in the path. -}
countElements : Path -> Int
countElements path =
  case path of
    Base -> 0
    Path s -> pathStructCount s


{-| Synonym for countElements. -}
depth : Path -> Int
depth = countElements


{-| Get the last element of the path. Synonym for last. NOTE : _O(1)_ -}
top : Path -> Maybe String
top path =
  case path of
    Base -> Nothing
    Path s -> pathStructTop s


{-| Get the last element of the path. NOTE : _O(1)_ -}
last : Path -> Maybe String
last = top


{-| Get first element of the path. NOTE: _O(n)_ -}
first : Path -> Maybe String
first path =
  case path of
    Base -> Nothing
    Path s ->
      if s.count > 0 then
        List.foldl (\elem _ -> Just elem) Nothing s.stack
      else
        Nothing


{-| Push an element to the end of the path. Synonym: `child` -}
push : String -> Path -> Path
push element path =
  case path of
    Base -> Path (pathStructOne element)
    Path s -> Path (pathStructPush element s)


{-| Pop an element from the end of the path. Synonym: `parent` -}
pop : Path -> Path
pop path =
  case path of
    Base -> path
    Path s -> if s.count <= 1 then Base else Path (pathStructPop s)


{-| Append a new last element to the path. Synonym: `push` -}
child : String -> Path -> Path
child = push


{-| Remove the last element from the path. Synonym: `pop` -}
parent : Path -> Path
parent = pop


{-| Interpret the path as a list of strings ordered from base to top. -}
toList : Path -> List String
toList path =
  case path of
    Base -> []
    Path s -> pathStructList s

{-| Get the internal stack. -}
toStack : Path -> List String
toStack path =
  case path of
    Base -> []
    Path s -> s.stack


{-| Set the internal stack. -}
fromStack : List String -> Path
fromStack stack =
  case stack of
    [] -> Base
    _ -> Path (pathStructFromStack stack)


{-| Create a path from a list of strings, ordered from the base to the top. -}
fromList : List String -> Path
fromList parts =
  case List.reverse parts of
    [] -> Base
    ls -> Path (pathStructFromList ls)


{-| Join the path elements with a glue delimiter, like `List.join`. -}
join : String -> Path -> String
join str path =
  case path of
    Base -> ""
    Path s -> pathStructJoin str s


{-| Go up in the path by _n_ parents. Some equivalencies:

  Path.up 1 mypath == Path.parent mypath == Path.pop mypath
  Path.up (Path.countElements mypath) mypath = Path.base

Synonym : `popMany`
-}
up : Int -> Path -> Path
up = popMany


{-| Synonym for `up`. -}
popMany : Int -> Path -> Path
popMany count path =
  if count >= countElements path then
    Base
  else
    popMany' count path
    |> trampoline


popMany' : Int -> Path -> Trampoline Path
popMany' n path =
  if n > 0 then
    case top path of
      Just topElement ->
        let path' = pop path
            n' = n - 1
        in if n' > 0 then
          Continue (\() -> popMany' n' path') else Done path'
      Nothing ->
        Done path
  else
    Done path


{-| Truncate the path to _n_ members by popping until that element count is attained. -}
truncate : Int -> Path -> Path
truncate nkeep path =
  case path of
    Base -> path
    Path s -> popMany (max (s.count - nkeep) 0) path


{-| Fold over a path's elements from the _base_. -}
foldBase : (String -> typeout -> typeout) -> typeout -> Path -> typeout
foldBase f inp path =
  case path of
    Base -> inp
    Path s -> pathStructFoldBase f inp s


{-| Fold over a path's elements from the _top_. -}
foldTop : (String -> typeout -> typeout) -> typeout -> Path -> typeout
foldTop f inp path =
  case path of
    Base -> inp
    Path s -> pathStructFoldTop f inp s


{-| Take two paths, setting the first as the child path of the second. -}
childPath : Path -> Path -> Path
childPath subPath basePath =
  case (subPath, basePath) of
    (Base, Base) -> Base
    (Path _, Base) -> subPath
    (Base, Path _) -> basePath
    (Path _, Path base) ->
      foldBase
        (\elem {count, stack} ->
          { count = count + 1
          , stack = elem :: stack
          }
        ) base subPath
      |> Path


{-| Given an ElmFire location and a path adjust the location relative to it's current position. -}
childLocation : ElmFire.Location -> Path -> ElmFire.Location
childLocation =
  foldBase ElmFire.sub


{-| Given the ElmFire root location used by your application (it will be rooted just in case) and a
path, give a location that represents it as an absolute path in to your database. -}
absoluteLocation : ElmFire.Location -> Path -> ElmFire.Location
absoluteLocation =
  ElmFire.root >> childLocation


{-| Compare two paths for equality. -}
isSame : Path -> Path -> Bool
isSame a b =
  isSame' (toList a) (toList b)
  |> trampoline


isSame' : List String -> List String -> Trampoline Bool
isSame' list_a list_b =
  case (list_a, list_b) of
    (a :: list_a', b :: list_b') ->
      if a /= b then Done False
      else Continue (\() -> isSame' list_a' list_b')
    (a :: list_a', []) -> Done False
    ([], b :: list_b') -> Done False
    ([], []) -> Done True
