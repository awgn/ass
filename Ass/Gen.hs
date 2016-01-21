-- Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
--
-- gen: C++ code ass'istant for vim


{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Main where

import System.Environment(getArgs)
import Ass.Cpp.Lang



-- Generator entities

data Entity = ENamespace     Identifier |
              SimpleClass    Identifier |
              TemplateClass  Identifier |
              MoveableClass  Identifier |
              ValueClass     Identifier |
              SingletonClass Identifier |
              CRTPClass      Identifier Identifier |
              YatsTest       String     |
              Streamable     Identifier |
              Showable       Identifier |
              Readable       Identifier
                deriving (Show)

factoryEntity :: Code -> [String] -> (Entity, [String])

factoryEntity 'c' (x:xs)    = (SimpleClass x, xs)
factoryEntity 'm' (x:xs)    = (MoveableClass x, xs)
factoryEntity 't' (x:xs)    = (TemplateClass  x, xs)
factoryEntity 'v' (x:xs)    = (ValueClass x, xs)
factoryEntity 's' (x:xs)    = (SingletonClass x, xs)
factoryEntity 'n' (x:xs)    = (ENamespace x, xs)
factoryEntity 'r' (x:y:xs)  = (CRTPClass x y, xs)
factoryEntity 'y' (x:xs)    = (YatsTest x, xs)
factoryEntity 'x' (x:xs)    = (Streamable x, xs)
factoryEntity 'S' (x:xs)    = (Showable x, xs)
factoryEntity 'R' (x:xs)    = (Readable x, xs)


factoryEntity  c  _ | c `elem` "cmtvsnryx" = error $ "Missing argument(s) for entity '" ++ [c] ++ "'"
factoryEntity  c  _ = error $ "Unknown entity '" ++ [c] ++ "'. Usage [code] ARG... \n" ++ helpString


helpString :: String
helpString = "     n  ID       namespace\n" ++
             "     c  ID       simple class\n" ++
             "     m  ID       moavable class\n" ++
             "     t  ID       template class\n" ++
             "     v  ID       value class\n" ++
             "     s  ID       singleton class\n" ++
             "     r  BASE ID  crtp idiom\n" ++
             "     y  ID       yats test\n" ++
             "     x  ID       streamable type\n" ++
             "     S  ID       show type\n" ++
             "     R  ID       read type"

---------------------------------- Cpp Class Models:

model :: Entity -> [CppEntity]

model (ENamespace name)  =
    cpp [ Namespace name [] ]


model (Showable name) =
    cpp
    [
        function
        (FuncDecl [Inline] string "show" (add_lvalue_reference . add_const $ Arg (Type name) "value"))
        (Body [""])
    ]

model (Readable name) =
    cpp
    [
        Function (Just $ Template[Typename "CharT", Typename "Traits"])
            (FuncDecl [] (Type "void") "read" (add_lvalue_reference $ Arg (Type name) "ret",
                                               Arg (Type "std::basic_istream<CharT, Traits> &") "in" ))
            (Body ["ret = ...;"])
    ]

model (SimpleClass name) =
    cpp
    [
        Class name NoBaseSpec
        [
             public
             [
                 ctor       name Unqualified,
                 dtor       name Unqualified,
                 copyCtor   name Delete,
                 operAssign name Delete
             ]
        ]
     ] ++
     cpp
     [
         operInsrt Nothing name,
         operExtrc Nothing name
     ]

model (MoveableClass name) =
    cpp
    [     Class name NoBaseSpec
          [
               public
               [
                   ctor name Unqualified,
                   dtor name Unqualified,
                   copyCtor name Delete,
                   operAssign name Delete,
                   moveCtor name Unqualified,
                   operMoveAssign name Unqualified
               ]
          ]
    ] ++
    cpp
    [
       operInsrt Nothing name,
       operExtrc Nothing name
    ]

model (ValueClass name) =
    cpp
    [
        Class name NoBaseSpec
        [
              public
              [
                  ctor name Unqualified,
                  dtor name Unqualified,
                  copyCtor name Unqualified,
                  operAssign name Unqualified
              ]
        ]
    ] ++
    cpp
    [
         operEq Nothing name,
         operNotEq Nothing name,
         operInsrt Nothing name,
         operExtrc Nothing name,
         operLt Nothing name,
         operLtEq Nothing name,
         operGt Nothing name,
         operGtEq Nothing name
    ]

-- Meyers' Singleton
model (SingletonClass name) =
    cpp
    [
        Class name NoBaseSpec
        [
            private
            [
                  ctor name Unqualified,
                  dtor name Unqualified
            ],
            public
            [
                  copyCtor   name Delete,
                  operAssign name Delete,
                  function
                      (FuncDecl [Static] (add_lvalue_reference (Type name)) "instance" ())
                      (MembBody [ "static " ++ name ++ " one;",
                                  "return one;" ] Unqualified)
            ]
        ]
    ]

-- Template Class
model (TemplateClass name) =
    cpp
    [
        Template [Typename "T"] +++
        Class name NoBaseSpec
        [
            public
            [
               ctor         name Unqualified,
               dtor         name Unqualified,
               copyCtor     name Delete,
               operAssign   name Delete
           ]
        ]
    ] ++
    cpp
    [
        operInsrt (Just $ Template [Typename "T"]) name,
        operExtrc (Just $ Template [Typename "T"]) name
    ]


model (CRTPClass base name) =
    cpp
    [
        Template [Typename "T"] +++
        Class base NoBaseSpec
        [
            public
            [
                ctor      base Unqualified,
                dtor      base Unqualified,
                copyCtor  base Delete,
                operAssign base Delete
            ]
        ]
    ] ++

    cpp
    [
        Class name (BaseSpecList [ public [ R (base ++ "<" ++ name ++ ">") ]])
        [
            public
            [
                ctor name Unqualified,
                dtor name Unqualified
            ]
        ]
    ]

model (YatsTest name) =
    cpp [ Include "yats.hpp"    ] ++
    cpp [ UsingNamespace "yats" ] ++
    cpp
    [
        R ("Context(" ++ name ++ ")" ),
        R "{\n}\n"
    ] ++
    cpp
    [
        _main [ "return yats::run(argc, argv);" ]
    ]

model (Streamable name) =
    cpp
    [
        operInsrt Nothing name,
        operExtrc Nothing name
    ]

----------------------------------

main :: IO ()
main = getArgs >>= runRender

-- "entities" arg0 arg1 arg2 ....
--

runRender :: [String] -> IO ()
runRender [] = return ()
runRender ("":_) = return ()
runRender (es:as) = do
                    let (e, as') = renderCode (head es) as
                    putStrLn $ render (model e)
                    runRender (tail es : as')

renderCode :: Code -> [String] -> (Entity, [String])
renderCode = factoryEntity

