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
    prettyShow xs = intercalate ", " (map prettyShow xs)

instance (CppShow a) => CppShow (Maybe a) where
    prettyShow (Just x) = prettyShow x
    prettyShow (Nothing) = ""


-- Alias types
--

type Identifier = String
type Type       = String
type Value      = String

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


-- Template
--

data TemplateParam = Typename String |
                     NonType String String |
                     TempTempParam String String
                        deriving (Show, Read)

instance CppShow TemplateParam where
    prettyShow (Typename name) = "typename " ++ name
    prettyShow (NonType name value) = name ++ " " ++ value
    prettyShow (TempTempParam name value) = name ++ " " ++ value 


newtype Template = Template [TemplateParam]
                    deriving (Show, Read)

instance CppShow Template where
    prettyShow (Template xs) = "template <" ++ (intercalate ", " $ map prettyShow xs) ++ ">"


newtype SpecializedTemplate = SpecializedTemplate [Value]
                                deriving (Show, Read)

instance CppShow SpecializedTemplate where
    prettyShow (SpecializedTemplate xs) = "<" ++ (intercalate ", " xs) ++ ">"


-- Fuctions Declaration
--

data FuncDecl = FuncDecl [Specifier] Type Identifier [Argument]
                    deriving (Show, Read)

instance CppShow FuncDecl where
    prettyShow (FuncDecl sp ty name as) = "\n" ++ spec ++ ( if(null spec) then "" else " ") ++ ty ++ 
                                        ( if (null spec && null ty) then "" else "\n" ) ++
                                            name ++ "("  ++ prettyShow(as) ++ ")" 
                                            where spec = prettyShow sp

-- Fuctions Body
--
   
data FuncBody = FuncBody [String] | MembFuncBody [String] (Maybe Qualifier)
                        deriving (Show, Read)

instance CppShow FuncBody where
    prettyShow (FuncBody xs) =  "\n{" ++ bodyToString xs ++ "}"
                                        where bodyToString [] = []
                                              bodyToString ys = intercalate "\n    " ( "" :ys) ++ "\n"
    prettyShow (MembFuncBody xs Nothing)        = prettyShow $ FuncBody xs
    prettyShow (MembFuncBody xs (Just Const))   = " const" ++ (prettyShow $ FuncBody xs)
    prettyShow (MembFuncBody xs (Just Volatile))= " volatile" ++ (prettyShow $ FuncBody xs)
    prettyShow (MembFuncBody _ (Just Delete))   = " = delete;"
    prettyShow (MembFuncBody _ (Just Default))  = " = default;" 
    prettyShow (MembFuncBody _ (Just Pure))     = " = 0;"


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
                OpGtEq Identifier |
                OpInsrt Identifier|    
                OpExtrc Identifier    
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
                        (OpInsrt xs)  -> prettyShow $ 
                                         Function (FuncDecl [] "template <typename CharT, typename Traits>\ntypename std::basic_ostream<CharT, Traits> &" 
                                                  "operator<<" [Named "std::basic_ostream<CharT,Traits>&" "out", Named ("const " ++ xs ++ "&") "that"])
                                                  (FuncBody ["return out;"])
                        (OpExtrc xs)  -> prettyShow $ 
                                         Function (FuncDecl [] "template <typename CharT, typename Traits>\ntypename std::basic_istream<CharT, Traits> &" 
                                                   "operator>>" [Named "std::basic_istream<CharT,Traits>&" "in", Named (xs ++ "&") "that"])
                                                   (FuncBody ["return in;"])
                        (Ctor name q)  -> prettyShow $ 
                                         Function (FuncDecl [] "" name [])
                                                  (MembFuncBody [] q)
                        (Dtor name xs q) -> prettyShow $ 
                                         Function (FuncDecl xs "" ("~" ++ name) []) 
                                                  (MembFuncBody [] q)
                        (CopyCtor name q)-> prettyShow $ 
                                         Function (FuncDecl [] "" name 
                                                  [Named (constLvalRef name) "other"]) 
                                                  (MembFuncBody [] q)
                        (OpAssign name q)-> prettyShow $ 
                                         Function (FuncDecl [] (lvalRef name) 
                                                  "operator=" [Named (constLvalRef name) "other"]) 
                                                  (MembFuncBody ["return *this;"] q)
                        (MoveCtor name q)-> prettyShow $ 
                                         Function (FuncDecl [] "" name 
                                                  [Named (rvalRef name) "other"]) 
                                                  (MembFuncBody [] q)
                        (OpMoveAssign name q) -> prettyShow $ 
                                         Function (FuncDecl [] (lvalRef name) "operator=" 
                                                  [Named (rvalRef name) "other"]) 
                                                  (MembFuncBody ["return *this;"] q)

-- List of Functions...
--

data MemberFunctions = Public [Function]      |
                       Protected [Function]   |
                       Private [Function]     
                            deriving (Show, Read)

instance CppShow MemberFunctions where
    prettyShow (Public xs)    = "\npublic:\n" ++ intercalateFunctions xs
    prettyShow (Protected xs) = "\nprotected:\n" ++ intercalateFunctions xs
    prettyShow (Private xs)   = "\nprivate:\n" ++ intercalateFunctions xs


data Functions = Free [Function]
                        deriving (Show, Read)

instance CppShow Functions where
    prettyShow (Free xs) =  intercalateFunctions xs                      
    
intercalateFunctions :: [Function] -> String  
intercalateFunctions xs =  if (null xs) then "" else (intercalate "\n" $ map prettyShow xs)  

-- Cpp class, including free functions operating on it
--
    
data Class = Class (Maybe Template) Identifier [MemberFunctions] 
                deriving (Show, Read)

instance CppShow Class where
    prettyShow (Class tp name fs) =  template ++ (if (null template) then "" else "\n")  ++ "class " ++ name ++ " {\n" ++
                                  (intercalate "\n" $ map prettyShow fs) ++ "\n\n};\n" 
                                    where template = prettyShow tp

-- Generator entities
--

data Entity = C  Identifier |
              SC Identifier |
              TC Template Identifier |     
              MC Identifier |
              VC Identifier |   
              S  Identifier    
                deriving (Show, Read)

instance CppShow Entity where
    -- Class
    prettyShow (C name)  = prettyShow $ 
                        Class Nothing name [
                        Public [Ctor name Nothing, 
                                Dtor name [] Nothing, 
                                CopyCtor name (Just Delete), 
                                OpAssign name (Just Delete)]] 
    -- Streamable Class
    prettyShow (SC name)  = prettyShow( 
                         Class Nothing name [
                         Public [Ctor name Nothing, 
                                 Dtor name [] Nothing, 
                                 CopyCtor name (Just Delete), 
                                 OpAssign name (Just Delete)]]) ++ 
                         prettyShow (Free [OpInsrt name, OpExtrc name])
    -- Template Class
    prettyShow (TC tmpl name)  = prettyShow ( 
                              Class (Just tmpl) name 
                              [Public [Ctor name Nothing, 
                                       Dtor name [] Nothing, 
                                       CopyCtor name (Just Delete), 
                                       OpAssign name (Just Delete)]]) ++ 
                               prettyShow (Free [OpInsrt name, OpExtrc name])
    -- MoveableClass
    prettyShow (MC name) = prettyShow $ 
                        Class Nothing name [
                        Public [Ctor name Nothing, 
                                Dtor name [] Nothing, 
                                CopyCtor name (Just Delete), 
                                OpAssign name (Just Delete), 
                                MoveCtor name Nothing, 
                                OpMoveAssign name Nothing]]  
    -- ValueClass
    prettyShow (VC name) = prettyShow( 
                        Class Nothing name [
                        Public [Ctor name Nothing, 
                                Dtor name [] Nothing, 
                                CopyCtor name Nothing, 
                                OpAssign name Nothing]]) ++ 
                        prettyShow (Free [OpEq name, OpNotEq name, OpInsrt name, OpExtrc name, 
                                          OpLt name, OpLtEq name, OpGt name, OpGtEq name])
    -- Singleton
    prettyShow (S name) = prettyShow $ 
                       Class Nothing name [
                        Private [Ctor name Nothing, 
                                 Dtor name [] Nothing],
                        Public  [CopyCtor name (Just Delete), 
                                 OpAssign name (Just Delete),
                                 Function (FuncDecl [Static] (name ++ "&") "instance" []) 
                                          (MembFuncBody  [ "static " ++ name ++ " one;", "return one;" ] Nothing)
                                 ]]
main :: IO ()
main = do
      args <- getArgs
      putStrLn $ prettyShow ((read $ unwords args ) :: Entity)
