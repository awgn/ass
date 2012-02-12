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


import Data.List
import System(getArgs)

-- CppShow type class
--

class CppShow a where
    prettyShow :: a -> String 

-- List of CppShow instance data
--
instance (CppShow a) => CppShow [a] where
    prettyShow xs = intercalate " " (map prettyShow xs)

-- Alias types
--

type Identifier = String
type Type       = String

-- A very approximate specifier and qualifiers for (member) functions... 
--

data Specifier = Inline | Static | Virtual | Constexpr 
                    deriving (Show, Read)
 
instance CppShow Specifier where
    prettyShow Inline    = "inline"
    prettyShow Static    = "static"
    prettyShow Virtual   = "virtual"
    prettyShow Constexpr = "constexpr"

data Qualifier = Const | Volatile | Delete | Default | Pure 
                    deriving (Show, Read)

instance CppShow Qualifier where
    prettyShow Const     = "const"
    prettyShow Volatile  = "volatile"
    prettyShow Delete    = "delete"
    prettyShow Default   = "default"
    prettyShow Pure      = "0"
 
-- Fuctions Argument
--

data Argument = Unnamed Type | Named Type String
                deriving (Show, Read)

instance CppShow Argument where
    prettyShow (Unnamed xs)   = xs
    prettyShow (Named xs ns)  = xs ++ " " ++ ns

-- Fuctions Declaration
--

data FuncDecl = FuncDecl [Specifier] Type Identifier [Argument]
                    deriving (Show, Read)

instance CppShow FuncDecl where
    prettyShow (FuncDecl sp ty id as) = "\n" ++ spec ++ ( if(null spec) then "" else " ") ++ ty ++ 
                                        ( if (null spec && null ty) then "" else "\n" ) ++
                                        id ++ "("  ++ (intercalate ", " $ map prettyShow as) ++ ")" 
                                            where spec = prettyShow sp

-- Fuctions Body
--
   
data FuncBody = FuncBody [String] | MembFuncBody [String] (Maybe Qualifier)
                        deriving (Show, Read)

instance CppShow FuncBody where
    prettyShow (FuncBody xs) =  "\n{" ++ bodyToString xs ++ "}"
                                        where bodyToString [] = []
                                              bodyToString xs = intercalate "\n    " ( "" :xs) ++ "\n"
    prettyShow (MembFuncBody xs Nothing) = prettyShow $ FuncBody xs
    prettyShow (MembFuncBody xs (Just Const))   = " const" ++ (prettyShow $ FuncBody xs)
    prettyShow (MembFuncBody xs (Just Volatile))= " volatile" ++ (prettyShow $ FuncBody xs)
    prettyShow (MembFuncBody xs (Just Delete))  = " = delete;"
    prettyShow (MembFuncBody xs (Just Default)) = " = default;" 
    prettyShow (MembFuncBody xs (Just Pure))    = " = 0;"


-- Some predefined Fuctions
--

data Function = Function FuncDecl FuncBody  |
                Ctor Identifier (Maybe Qualifier) |
                Dtor Identifier [Specifier] (Maybe Qualifier) | 
                CopyCtor Identifier (Maybe Qualifier) |
                MoveCtor Identifier (Maybe Qualifier) |
                OpAssign  Identifier (Maybe Qualifier) |
                OpMoveAssign Identifier (Maybe Qualifier) |
                OpEq Identifier |
                OpNotEq Identifier |    
                OpLt Identifier |
                OpLtEq Identifier |
                OpGt Identifier |
                OpGtEq Identifier 
                        deriving (Show, Read)
 
instance CppShow Function where
    prettyShow x = let  lhs name = Named ("const " ++ name ++ "&") "lhs"
                        rhs name = Named ("const " ++ name ++ "&") "rhs"
                        lvalRef xs = xs ++ "&"
                        constLvalRef xs = "const " ++ xs ++ "&"
                        rvalRef xs = xs ++ "&&"
                       in case x of
                               (Function decl body) -> (prettyShow decl) ++ (prettyShow body) 
                               (OpEq xs)    -> prettyShow $ Function (FuncDecl [Inline] "bool" "operator==" [lhs xs, rhs xs]) 
                                                                     (FuncBody ["/* implementation */" ])
                               (OpNotEq xs) -> prettyShow $ Function (FuncDecl [Inline] "bool" "operator!=" [lhs xs, rhs xs]) 
                                                                     (FuncBody ["return !(lhs == rhs);"])
                               (OpLt xs)    -> prettyShow $ Function (FuncDecl [Inline] "bool" "operator<"  [lhs xs, rhs xs]) 
                                                                     (FuncBody ["/* implementation */"] )
                               (OpLtEq xs)  -> prettyShow $ Function (FuncDecl [Inline] "bool" "operator<=" [lhs xs, rhs xs]) 
                                                                     (FuncBody ["return !(rhs < lhs);"] )
                               (OpGt xs)    -> prettyShow $ Function (FuncDecl [Inline] "bool" "operator>"  [lhs xs, rhs xs]) 
                                                                     (FuncBody ["return rhs < lhs;"]    )
                               (OpGtEq xs)  -> prettyShow $ Function (FuncDecl [Inline] "bool" "operator>=" [lhs xs, rhs xs]) 
                                                                     (FuncBody ["return !(lsh < rhs);"] )
                               (Ctor id q)  -> prettyShow $ Function (FuncDecl [] "" id []) 
                                                                     (MembFuncBody [] q)
                               (Dtor id xs q) -> prettyShow $ Function (FuncDecl xs "" ("~" ++ id) []) 
                                                                       (MembFuncBody [] q)
                               (CopyCtor id q)-> prettyShow $ Function (FuncDecl [] "" id [Named (constLvalRef id) "other"]) 
                                                                       (MembFuncBody [] q)
                               (OpAssign id q)-> prettyShow $ Function (FuncDecl [] (lvalRef id) "operator=" [Named (constLvalRef id) "other"]) 
                                                                       (MembFuncBody ["return *this;"] q)
                               (MoveCtor id q)-> prettyShow $ Function (FuncDecl [] "" id [Named (rvalRef id) "other"]) 
                                                                       (MembFuncBody [] q)
                               (OpMoveAssign id q) -> prettyShow $ Function (FuncDecl [] (lvalRef id) "operator=" [Named (rvalRef id) "other"]) 
                                                                            (MembFuncBody ["return *this;"] q)

-- List of Functions...
--

data Functions = Public [Function]      |
                 Protected [Function]   |
                 Private [Function]     |
                 Free [Function]
                    deriving (Show, Read)

instance CppShow Functions where
    prettyShow (Free   xs)    =  intercalateFunctions xs                      
    prettyShow (Public xs)    = "\npublic:\n" ++ intercalateFunctions xs
    prettyShow (Protected xs) = "\nprotected:\n" ++ intercalateFunctions xs
    prettyShow (Private xs)   = "\nprivate:\n" ++ intercalateFunctions xs
    
intercalateFunctions xs =  if (null xs) then "" else (intercalate "\n" $ map prettyShow xs)  

-- Cpp class, including free functions operating on it
--
    
data Class = RawClass Identifier [Functions] Functions |
             Class Identifier |
             MoveableClass Identifier |
             ValueClass Identifier |   
             ValueClass' Identifier |
             Singleton Identifier    
                deriving (Show, Read)

instance CppShow Class where
    prettyShow (RawClass id fs xs) = "class " ++ id ++ " {\n" ++
                                        (intercalate "\n" $ map prettyShow fs) ++
                                         "\n\n};\n" ++
                                            prettyShow xs
    -- Class
    prettyShow (Class id)  = prettyShow $ 
            RawClass id [Public [Ctor id Nothing, 
                                 Dtor id [] Nothing, 
                                 CopyCtor id (Just Delete), 
                                 OpAssign id (Just Delete)]] (Free [])
    -- MoveableClass
    prettyShow (MoveableClass id) = prettyShow $ 
            RawClass id [Public [Ctor id Nothing, 
                                 Dtor id [] Nothing, 
                                 CopyCtor id (Just Delete), 
                                 OpAssign id (Just Delete), 
                                 MoveCtor id Nothing, 
                                 OpMoveAssign id Nothing]] (Free [])
    -- ValueClass
    prettyShow (ValueClass id) = prettyShow $ 
            RawClass id [Public [Ctor id Nothing, 
                                 Dtor id [] Nothing, 
                                 CopyCtor id Nothing, 
                                 OpAssign id Nothing]] (Free [OpEq id, OpNotEq id])
    -- ValueClass'
    prettyShow (ValueClass' id) = prettyShow $ 
            RawClass id [Public [Ctor id Nothing, 
                                 Dtor id [] Nothing, 
                                 CopyCtor id Nothing, 
                                 OpAssign id Nothing]] (Free [OpEq id, OpNotEq id, OpLt id, OpLtEq id, OpGt id, OpGtEq id])
    -- Singleton
    prettyShow (Singleton id) = prettyShow $ 
            RawClass id [Private [Ctor id Nothing, 
                                  Dtor id [] Nothing],
                         Public  [CopyCtor id (Just Delete), 
                                  OpAssign id (Just Delete),
                                  Function (FuncDecl [Static] (id ++ "&") "instance" [] ) 
                                           (MembFuncBody  [ "static " ++ id ++ " one;", "return one;" ] Nothing)
                                  ]] (Free [])

main :: IO ()
main = do
      args <- getArgs
      putStrLn $ prettyShow ((read $ unwords args ) :: Class)
