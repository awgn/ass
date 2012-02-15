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
import Data.Monoid
import System(getArgs)
import Control.Applicative

-- CppShow type class
--

class CppShow a where
    render :: a -> String 
    

-- List of CppShow instance data
--

instance (CppShow a) => CppShow (Maybe a) where
    render (Just x) = render x
    render (Nothing) = ""

instance (CppShow a) => CppShow [a] where
    render xs =  unwords $ render <$> xs

newtype CommaSep a = CommaSep [a]    
                    deriving (Show, Read)

instance (CppShow a) => CppShow (CommaSep a) where
    render (CommaSep xs) = intercalate ", " $ render <$> xs  

-- Alias types
--

type Identifier = String
type Type       = String

-- A very approximate specifier and qualifiers for (member) functions... 
--

data Specifier = Inline | Static | Virtual | Constexpr 
                    deriving (Show, Read)
 
instance CppShow Specifier where
    render Inline    = "inline"
    render Static    = "static"
    render Virtual   = "virtual"
    render Constexpr = "constexpr"

data Qualifier = Constant | Volatile | Delete | Default | Pure 
                    deriving (Show, Read)

instance CppShow Qualifier where
    render Constant  = "const"
    render Volatile  = "volatile"
    render Delete    = "delete"
    render Default   = "default"
    render Pure      = "0"
 
-- Fuctions Argument
--

data Argument = Unnamed Type | Named Type String
                deriving (Show, Read)

instance CppShow Argument where
    render (Unnamed xs)   = xs
    render (Named xs ns)  = xs ++ " " ++ ns

-- Template
--

data TemplateParam = Typename { name :: String } |
                     NonType  { type' :: String,  name :: String } |
                     TempTempParam { type' :: String, name :: String }
                        deriving (Show, Read)

instance CppShow TemplateParam where
    render (Typename ns) = "typename " ++ ns
    render (NonType ns value) = ns ++ " " ++ value
    render (TempTempParam ns value) = ns ++ " " ++ value 


newtype Template = Template [TemplateParam]
                    deriving (Show, Read)

instance CppShow Template where
    render (Template xs) = "template <" ++ render (CommaSep xs) ++ ">"

-- Template is an instance of Monoid!
--
instance Monoid Template where
    mempty = Template []
    mappend (Template xs) (Template ys) = Template (mappend xs ys)


specializeName :: Maybe Template -> String -> String
specializeName Nothing ns = ns
specializeName (Just (Template xs)) ns = ns ++ "<" ++ (intercalate ", " (map name xs) ) ++ ">"


-- Fuctions Declaration
--

data FuncDecl = FuncDecl [Specifier] Type Identifier (CommaSep Argument)
                    deriving (Show, Read)

instance CppShow FuncDecl where
    render (FuncDecl sp ty ns args) = "\n" ++ spec ++ ( if(null spec ) then "" else " ") ++ ty ++ 
                                        ( if (null spec && null ty) then "" else "\n" ) ++
                                        ns ++ "("  ++ render(args) ++ ")" 
                                            where spec = render sp
-- Fuctions Body
--
   
data FuncBody = FuncBody [String] | MembFuncBody [String] (Maybe Qualifier)
                    deriving (Show, Read)

instance CppShow FuncBody where
    render (FuncBody xs) =  "\n{" ++ bodyToString xs ++ "}"
                                        where bodyToString [] = []
                                              bodyToString ys = intercalate "\n    " ( "" :ys) ++ "\n"
    render (MembFuncBody xs Nothing)         = render $ FuncBody xs
    render (MembFuncBody xs (Just Constant)) = " const" ++ (render $ FuncBody xs)
    render (MembFuncBody xs (Just Volatile)) = " volatile" ++ (render $ FuncBody xs)
    render (MembFuncBody _ (Just Delete))    = " = delete;"
    render (MembFuncBody _ (Just Default))   = " = default;" 
    render (MembFuncBody _ (Just Pure))      = " = 0;"


-- Fuction
--

data Function = Function (Maybe Template) FuncDecl FuncBody  
                    deriving (Show, Read)

instance CppShow Function where
    render (Function tp decl body) = render tp ++ render decl ++ render body 

-- Helper functions
--

add_lvalue_ref :: Argument -> Argument
add_lvalue_ref (Unnamed xs)  = Unnamed (xs ++ "&")
add_lvalue_ref (Named xs ys) = Named (xs ++ "&") ys

add_rvalue_ref :: Argument -> Argument
add_rvalue_ref (Unnamed xs) = Unnamed (xs ++ "&&")
add_rvalue_ref (Named xs ys) = Named (xs ++ "&&") ys

add_const_lvalue_ref :: Argument -> Argument
add_const_lvalue_ref (Unnamed xs) = Unnamed( "const " ++ xs ++ "&")
add_const_lvalue_ref (Named xs ys) = Named ("const " ++ xs ++ "&") ys


-- Predefined Functions
--

ctor :: [Specifier] -> Identifier -> Maybe Qualifier -> Function
ctor spec ns qual = Function Nothing (FuncDecl spec "" ns (CommaSep [])) (MembFuncBody [] qual)


dtor :: [Specifier] -> Identifier -> Maybe Qualifier -> Function
dtor spec ns qual = Function Nothing (FuncDecl spec "" ("~" ++ ns) (CommaSep [])) (MembFuncBody [] qual)


copyCtor :: [Specifier] -> Identifier -> Maybe Qualifier -> Function
copyCtor spec ns qual = Function Nothing (FuncDecl spec "" ns (CommaSep [add_const_lvalue_ref(Named ns "other")])) (MembFuncBody [] qual)


moveCtor :: [Specifier] -> Identifier -> Maybe Qualifier -> Function
moveCtor spec ns qual = Function Nothing (FuncDecl spec "" ns (CommaSep [add_rvalue_ref $ Named ns "other"])) (MembFuncBody [] qual)


operAssign:: [Specifier] -> Identifier -> Maybe Qualifier -> Function
operAssign spec ns qual = Function Nothing (FuncDecl spec (ns ++ "&") "operator=" (CommaSep [add_const_lvalue_ref $ Named ns "other"])) 
                                    (MembFuncBody ["return *this;" ] qual)

operMoveAssign:: [Specifier] -> Identifier -> Maybe Qualifier -> Function
operMoveAssign spec ns qual = Function Nothing (FuncDecl spec (ns ++ "&") "operator=" (CommaSep [add_rvalue_ref $ Named ns "other"])) 
                                        (MembFuncBody ["return *this;" ] qual)

-- Generic free-functions
--

operEq :: (Maybe Template) -> Identifier -> Function
operEq tp xs  = Function tp (FuncDecl [Inline] "bool" "operator==" 
                            (CommaSep [add_const_lvalue_ref(Named xs "lhs"), add_const_lvalue_ref(Named xs "rhs")])) 
                            (FuncBody ["/* implementation */" ])

operNotEq :: (Maybe Template) -> Identifier -> Function
operNotEq tp xs = Function tp (FuncDecl [Inline] "bool" "operator!=" 
                              (CommaSep [add_const_lvalue_ref(Named xs "lhs"), add_const_lvalue_ref(Named xs "rhs")])) 
                              (FuncBody ["return !(lhs == rhs);"])

operLt :: (Maybe Template) -> Identifier -> Function
operLt tp xs = Function tp (FuncDecl [Inline] "bool" "operator<"  
                           (CommaSep [add_const_lvalue_ref(Named xs "lhs"), add_const_lvalue_ref(Named xs "rhs")])) 
                           (FuncBody ["/* implementation */"])

operLtEq :: (Maybe Template) -> Identifier -> Function
operLtEq tp xs = Function tp (FuncDecl [Inline] "bool" "operator<=" 
                             (CommaSep [add_const_lvalue_ref(Named xs "lhs"), add_const_lvalue_ref(Named xs "rhs")])) 
                             (FuncBody ["return !(rhs < lhs);"])

operGt :: (Maybe Template) -> Identifier -> Function
operGt tp xs = Function tp (FuncDecl [Inline] "bool" "operator>"  
                           (CommaSep [add_const_lvalue_ref(Named xs "lhs"), add_const_lvalue_ref(Named xs "rhs")])) 
                           (FuncBody ["return rhs < lhs;"])

operGtEq :: (Maybe Template) -> Identifier -> Function
operGtEq tp xs = Function tp (FuncDecl [Inline] "bool" "operator>=" 
                             (CommaSep [add_const_lvalue_ref(Named xs "lhs"), add_const_lvalue_ref(Named xs "rhs")])) 
                             (FuncBody ["return !(lsh < rhs);"])

operInsrt :: (Maybe Template) -> Identifier -> Function
operInsrt tp xs = Function (tapp) (FuncDecl []  
                               "typename std::basic_ostream<CharT, Traits> &" 
                               "operator<<" 
                               (CommaSep [Named "std::basic_ostream<CharT,Traits>&" "out", add_const_lvalue_ref(Named (specializeName tp xs) "that")]))
                               (FuncBody ["return out;"])
                                    where tapp =  mappend (Just (Template[Typename "CharT", Typename "Traits"])) tp 

operExtrc :: (Maybe Template) -> Identifier -> Function
operExtrc tp xs = Function (tapp) (FuncDecl []  
                            "typename std::basic_istream<CharT, Traits> &" 
                            "operator>>" (CommaSep [Named "std::basic_istream<CharT,Traits>&" "in", add_lvalue_ref $ Named (specializeName tp xs) "that"]))
                            (FuncBody ["return in;"])
                                    where tapp =  mappend (Just (Template[Typename "CharT", Typename "Traits"])) tp 

-- List of Functions...
-- 

data MemberFunctions = Public [Function]      |
                       Protected [Function]   |
                       Private [Function]     
                          deriving (Show, Read)

instance CppShow MemberFunctions where
    render (Public xs)    = "\npublic:\n" ++ intercalateFunctions xs
    render (Protected xs) = "\nprotected:\n" ++ intercalateFunctions xs
    render (Private xs)   = "\nprivate:\n" ++ intercalateFunctions xs


data Functions = Free [Function]
                        deriving (Show, Read)

instance CppShow Functions where
    render (Free xs) =  intercalateFunctions xs                      

    
intercalateFunctions :: [Function] -> String  
intercalateFunctions xs =  if (null xs) then "" else (intercalate "\n" $ map render xs)  

-- Cpp class, including free functions operating on it
--
    
data Class = Class (Maybe Template) Identifier [MemberFunctions] 
                deriving (Show, Read)


instance CppShow Class where
    render (Class tp ns fs) =  template ++ (if (null template) then "" else "\n")  ++ "class " ++ ns ++ " {\n" ++
                                  (intercalate "\n" $ map render fs) ++ "\n\n};\n" 
                                    where template = render tp

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
    render (C ns)  = render $ 
                            Class Nothing ns [
                            Public [ctor [] ns Nothing, 
                                    dtor [] ns Nothing, 
                                    copyCtor [] ns (Just Delete), 
                                    operAssign [] ns (Just Delete)]] 
    -- Streamable Class
    render (SC ns)  = render( 
                            Class Nothing ns [
                            Public [ctor [] ns Nothing, 
                                    dtor [] ns Nothing, 
                                    copyCtor [] ns (Just Delete), 
                                    operAssign [] ns (Just Delete)]]) ++ 
                            render (Free [operInsrt Nothing ns, operExtrc Nothing ns])
    -- Template Class
    render (TC tp ns) = render ( 
                                Class (Just tp) ns [
                                Public [ctor [] ns Nothing, 
                                        dtor [] ns Nothing, 
                                        copyCtor [] ns (Just Delete), 
                                        operAssign [] ns (Just Delete)]]) ++ 
                                render (Free [operInsrt (Just tp) ns, 
                                              operExtrc (Just tp) ns])
    -- MoveableClass
    render (MC ns) = render $ 
                            Class Nothing ns [
                            Public [ctor [] ns Nothing, 
                                    dtor [] ns Nothing, 
                                    copyCtor [] ns (Just Delete), 
                                    operAssign [] ns (Just Delete), 
                                    moveCtor [] ns Nothing, 
                                    operMoveAssign [] ns Nothing]]  
    -- ValueClass
    render (VC ns) = render( 
                            Class Nothing ns [
                            Public [ctor [] ns Nothing, 
                                    dtor [] ns Nothing, 
                                    copyCtor [] ns Nothing, 
                                    operAssign [] ns Nothing]]) ++ 
                            render (Free [operEq Nothing ns, operNotEq Nothing ns, operInsrt Nothing ns, operExtrc Nothing ns, 
                                              operLt Nothing ns, operLtEq Nothing ns, operGt Nothing ns, operGtEq Nothing ns])
    -- Meyers' Singleton
    render (S ns) = render $ 
                          Class Nothing ns [
                            Private [ctor [] ns Nothing, 
                                     dtor [] ns Nothing],
                            Public [copyCtor [] ns (Just Delete), 
                                    operAssign [] ns (Just Delete),
                                    Function Nothing (FuncDecl [Static] (ns ++ "&") "instance" (CommaSep[])) 
                                             (MembFuncBody  [ "static " ++ ns ++ " one;", "return one;" ] Nothing)
                                  ]]
main :: IO ()
main = do
      args <- getArgs
      putStrLn $ render ((read $ unwords args) :: Entity)
