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

module Main where

import Data.List
import Data.Maybe
import Data.Monoid
import System.Environment
import Control.Applicative


type Identifier = String
type Code = Char


main :: IO ()
main = getArgs >>= runRender

-- "entities" arg0 arg1 arg2 ....
--

runRender :: [String] -> IO ()

runRender [] = return ()
runRender ("":_) = return ()
runRender (es:as) = do
                    let (e, (as')) = renderCode (es !! 0) as
                    putStrLn $ render (model e)
                    runRender (drop 1 es: as') 

renderCode :: Code -> [String] -> (Entity, [String])
renderCode x args = factoryEntity x args

-- Generator entities
--

data Entity = ENamespace     Identifier |
              SimpleClass    Identifier |
              TemplateClass  Identifier |     
              MoveableClass  Identifier |
              ValueClass     Identifier |   
              SingletonClass Identifier |
              YatsTest String
                deriving (Show)
                         

factoryEntity :: Code -> [String] -> (Entity, [String])

factoryEntity 'c' (x:xs) = (SimpleClass x, xs)
factoryEntity 'm' (x:xs) = (MoveableClass x, xs)
factoryEntity 't' (x:xs) = (TemplateClass  x, xs)
factoryEntity 'v' (x:xs) = (ValueClass x, xs)
factoryEntity 's' (x:xs) = (SingletonClass x, xs)
factoryEntity 'n' (x:xs) = (ENamespace x, xs)
factoryEntity 'y' (x:xs) = (YatsTest x, xs)
factoryEntity  x  [] = error $ "Gen: missing argument(s) for entity " ++ [x] 
factoryEntity  x  _  = error $ "Gen: unknown entity " ++ [x] 


---------------------------------------------------------
-- Cpp Class Models:                             

cpp :: (Show a, CppShow a) => [a] -> [CppEntity]
cpp = map CppEntity  

model :: Entity -> [CppEntity]

model (ENamespace name)  = cpp [ Namespace name [] ]

model (SimpleClass name) = cpp [ Class name [
                                    public 
                                    [
                                        ctor [] name Unqualified, 
                                        dtor [] name Unqualified, 
                                        copyCtor [] name Delete, 
                                        operAssign [] name Delete
                                    ]
                                 ]
                            ] ++ 
                            cpp 
                            [
                                operInsrt Nothing name, 
                                operExtrc Nothing name
                            ]

model (MoveableClass name) = cpp [ Class name [
                                        public 
                                        [
                                            ctor [] name Unqualified, 
                                            dtor [] name Unqualified, 
                                            copyCtor [] name Delete, 
                                            operAssign [] name Delete, 
                                            moveCtor [] name Unqualified, 
                                            operMoveAssign [] name Unqualified
                                        ]
                                    ]
                             ] ++
                             cpp [
                                operInsrt Nothing name, 
                                operExtrc Nothing name
                             ]

model (ValueClass name) = cpp [ Class name [
                                    public [
                                        ctor [] name Unqualified,  
                                        dtor [] name Unqualified, 
                                        copyCtor [] name Unqualified, 
                                        operAssign [] name Unqualified
                                    ]
                                ]
                          ] ++
                          cpp [
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
model (SingletonClass name) = cpp [ Class name [
                                        private [
                                            ctor [] name Unqualified, 
                                            dtor [] name Unqualified
                                        ],
                                        public [
                                            copyCtor [] name Delete, 
                                            operAssign [] name Delete,
                                            Function Nothing 
                                                (FuncDecl [Static] (Just (add_lvalue_reference (Type name))) "instance" (CommaSep[])) 
                                                (MembFuncBody [ "static " ++ name ++ " one;", "return one;" ] Unqualified)
                                        ]
                                  ]
                              ]
-- Template Class
model (TemplateClass name) = cpp [ Template [Typename "T"] +++ 
                                   Class name [
                                   public [
                                        ctor [] name Unqualified, 
                                        dtor [] name Unqualified, 
                                        copyCtor [] name Delete, 
                                        operAssign [] name Delete
                                        ]
                                   ]
                             ] ++
                             cpp [   
                                 operInsrt (Just $ Template [Typename "T"]) name, 
                                 operExtrc (Just $ Template [Typename "T"]) name
                             ]

model (YatsTest name) = cpp [ Include "yats.hpp" ] ++
                        cpp [ UsingNamespace "yats" ] ++
                        cpp [ Raw $ "Context(" ++ name ++ ")",
                              Raw $ "{\n}\n"
                        ] ++
                        cpp [ _main [ "return yats::run(argc, argv);" ] ]


---------------------------------------------------------
-- CppShow type class
--

class CppShow a where
    render :: a -> String 

-- Existential CppEntity 
--

data CppEntity = forall a. (CppShow a, Show a) => CppEntity a

instance  CppShow CppEntity where
    render (CppEntity xs) = render xs

instance Show CppEntity where
    show (CppEntity xs) = show xs

-- CommaSep list
--

newtype CommaSep a = CommaSep { getCommaSep :: [a] }   
                         deriving (Show)

-- (Maybe a) instance of CppShow 
--

instance (CppShow a) => CppShow (Maybe a) where
    render (Just x)  = render x
    render (Nothing) = ""

-- [a] instace of CppShow
--

instance (CppShow a) => CppShow [a] where
    render xs =  intercalate "\n" $ render <$> xs


-- (CommaSep a) instance of CppShow
--

instance (CppShow a) => CppShow (CommaSep a) where
    render (CommaSep xs) = intercalate ", " $ render <$> xs  


---------------------------------------------------------
-- Raw: Raw String for CppShow
--

newtype Raw = Raw { getString :: String }
                    deriving (Show)

instance CppShow Raw where
    render = getString

---------------------------------------------------------
-- Cpp Specifier and Qualifier:
-- a very approximate rendering for (member) functions... 
--

data Specifier = Inline | Static | Virtual | Constexpr 
                    deriving (Show)
 
instance CppShow Specifier where
    render Inline    = "inline"
    render Static    = "static"
    render Virtual   = "virtual"
    render Constexpr = "constexpr"


data Qualifier = Unqualified | Constant | Volatile | Delete | Default | Pure 
                    deriving (Show)

instance CppShow Qualifier where
    render Unqualified = ""
    render Constant  = "const"
    render Volatile  = "volatile"
    render Delete    = "delete"
    render Default   = "default"
    render Pure      = "0"

---------------------------------------------------------
-- Using directive and declaration

data Using = Using String | UsingNamespace String
                deriving (Show)

instance CppShow Using where
    render (Using member) = "using " ++ member ++ ";\n";
    render (UsingNamespace name) = "using namespace " ++ name ++ ";\n";


---------------------------------------------------------
-- Include directive

data Include = Include FilePath | RelativeInclude FilePath
                deriving (Show)

instance CppShow Include where
    render (Include file) = "#include <" ++ file ++ ">\n";
    render (RelativeInclude file) = "#include \"" ++ file ++ "\"\n";
               

---------------------------------------------------------
-- Cpp Types for Function Arguments
--             
--

class CppType a where
    getType              :: a -> String
    -- useful type_traits...
    add_cv               :: a -> a
    add_const            :: a -> a
    add_volatile         :: a -> a
    add_lvalue_reference :: a -> a
    add_rvalue_reference :: a -> a
    add_pointer          :: a -> a

newtype Type = Type String 
                deriving (Eq, Show)

instance CppShow Type where
    render t  = getType t

instance CppType Type where
    getType (Type xs) = xs
    add_cv               (Type xs) = Type $ xs ++ " const volatile"
    add_const            (Type xs) = Type $ xs ++ " const"
    add_volatile         (Type xs) = Type $ xs ++ " volatile"
    add_lvalue_reference (Type xs) = Type $ xs ++ "& "
    add_rvalue_reference (Type xs) = Type $ xs ++ "&& "
    add_pointer          (Type xs) = Type $ xs ++ "* "


data ArgType = Unnamed Type | Named Type Identifier 
                deriving (Show)

instance CppShow ArgType where
    render (Unnamed t)  = render t
    render (Named t ns) = (render t) ++ " " ++ ns

instance CppType ArgType where
    getType              (Unnamed t) = getType t
    getType              (Named t _) = getType t
    add_cv               (Unnamed t) = Unnamed (add_cv t) 
    add_cv               (Named t n) = Named   (add_cv t) n 
    add_const            (Unnamed t) = Unnamed (add_const t)
    add_const            (Named t n) = Named   (add_const t) n
    add_volatile         (Unnamed t) = Unnamed (add_volatile t)
    add_volatile         (Named t n) = Named   (add_volatile t) n
    add_lvalue_reference (Unnamed t) = Unnamed (add_lvalue_reference t)
    add_lvalue_reference (Named t n) = Named   (add_lvalue_reference t) n
    add_rvalue_reference (Unnamed t) = Unnamed (add_rvalue_reference t)
    add_rvalue_reference (Named t n) = Named   (add_rvalue_reference t) n
    add_pointer          (Unnamed t) = Unnamed (add_pointer t)
    add_pointer          (Named t n) = Named   (add_pointer t) n


add_const_lvalue_reference :: (CppType a) => a -> a
add_const_lvalue_reference = add_lvalue_reference . add_const

---------------------------------------------------------
-- Predefined Cpp member Functions
--

ctor :: [Specifier] -> Identifier -> Qualifier -> Function
ctor spec ns qual = Function Nothing (FuncDecl spec Nothing ns (CommaSep [])) (MembFuncBody [] qual)

dtor :: [Specifier] -> Identifier -> Qualifier -> Function
dtor spec ns qual = Function Nothing (FuncDecl spec Nothing ("~" ++ ns) (CommaSep [])) (MembFuncBody [] qual)


copyCtor :: [Specifier] -> Identifier -> Qualifier -> Function
copyCtor spec ns qual = Function Nothing (FuncDecl spec Nothing ns 
                            (CommaSep [add_const_lvalue_reference(Named (Type ns) "other")])) (MembFuncBody [] qual)


moveCtor :: [Specifier] -> Identifier -> Qualifier -> Function
moveCtor spec ns qual = Function Nothing (FuncDecl spec Nothing ns 
                            (CommaSep [add_rvalue_reference $ Named (Type ns) "other"])) (MembFuncBody [] qual)


operAssign:: [Specifier] -> Identifier -> Qualifier -> Function
operAssign spec ns qual = Function Nothing (FuncDecl spec (Just $ add_lvalue_reference (Type ns)) "operator=" 
                            (CommaSep [add_const_lvalue_reference $ Named (Type ns) "other"])) 
                                (MembFuncBody ["return *this;" ] qual)

operMoveAssign:: [Specifier] -> Identifier -> Qualifier -> Function
operMoveAssign spec ns qual = Function Nothing 
                              (FuncDecl spec (Just $ add_lvalue_reference (Type ns)) "operator=" 
                                (CommaSep [add_rvalue_reference $ Named (Type ns) "other"])) 
                                    (MembFuncBody ["return *this;" ] qual)


---------------------------------------------------------
-- Predefined Cpp free functions
--

_main :: [String] -> Function
_main impl =  Function Nothing 
                (FuncDecl [] (Just $ Type "int") "main" (CommaSep [Named (Type "int") "argc", Named (Type "char *") "argv[]" ]))
                (FuncBody impl )

operEq :: (Maybe Template) -> Identifier -> Function
operEq tp xs  = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator==" 
                            (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                            (FuncBody ["/* implementation */" ])        

operNotEq :: (Maybe Template) -> Identifier -> Function
operNotEq tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator!=" 
                              (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                              (FuncBody ["return !(lhs == rhs);"])

operLt :: (Maybe Template) -> Identifier -> Function
operLt tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator<"  
                           (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                           (FuncBody ["/* implementation */"])

operLtEq :: (Maybe Template) -> Identifier -> Function
operLtEq tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator<=" 
                             (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                             (FuncBody ["return !(rhs < lhs);"])

operGt :: (Maybe Template) -> Identifier -> Function
operGt tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator>"  
                            (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                            (FuncBody ["return rhs < lhs;"])

operGtEq :: (Maybe Template) -> Identifier -> Function
operGtEq tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator>=" 
                             (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                             (FuncBody ["return !(lsh < rhs);"])

operInsrt :: (Maybe Template) -> Identifier -> Function
operInsrt tp xs = Function (tapp) (FuncDecl []  
                           (Just (Type "typename std::basic_ostream<CharT, Traits> &")) 
                           "operator<<" 
                           (CommaSep [Named (Type "std::basic_ostream<CharT,Traits>&") "out", 
                                add_const_lvalue_reference (Named (Type $ getFullySpecializedName tp xs) "that")]))
                           (FuncBody ["return out;"])
                                where tapp =  mappend (Just (Template[Typename "CharT", Typename "Traits"])) tp 

operExtrc :: (Maybe Template) -> Identifier -> Function
operExtrc tp xs = Function (tapp) (FuncDecl []  
                           (Just (Type "typename std::basic_istream<CharT, Traits> &")) 
                           "operator>>" 
                           (CommaSep [Named (Type "std::basic_istream<CharT,Traits>&") "in", 
                                add_lvalue_reference $ Named (Type $ getFullySpecializedName tp xs) "that"]))
                           (FuncBody ["return in;"])
                                where tapp =  mappend (Just (Template[Typename "CharT", Typename "Traits"])) tp 


---------------------------------------------------------
-- Cpp Namespace 
--

data Namespace = Namespace Identifier [CppEntity]
                    deriving (Show)

instance CppShow Namespace where
    render (Namespace ns es)   = "namespace " ++ ns ++ " {\n" ++ 
                                    (render es) ++ 
                                    "\n} // namespace " ++ ns 

---------------------------------------------------------
-- Cpp Class, ClassEntities and CppEntities 
--

data Class = Class Identifier [ClassEntities] |
             TClass Template Identifier [ClassEntities]
                deriving (Show)


instance CppShow Class where
    render (Class ns es) =  "class " ++ ns ++ " {\n" ++ (intercalate "\n" $ map render es) ++ "\n\n};\n" 
    render (TClass tp ns es) =  template ++ (if (null template) then "" else "\n")  ++ "class " ++ ns ++ " {\n" ++
                                 (intercalate "\n" $ map render es) ++ "\n\n};\n" 
                                   where template = render tp


data ClassEntities = Public      [CppEntity] |
                     Protected   [CppEntity] |
                     Private     [CppEntity]
                        deriving (Show)


public :: (Show a, CppShow a) => [a] -> ClassEntities
public xs = Public $ map CppEntity xs  

protected :: (Show a, CppShow a) => [a] -> ClassEntities
protected xs = Protected $ map CppEntity xs  

private :: (Show a, CppShow a) => [a] -> ClassEntities
private xs = Private $ map CppEntity xs  


instance CppShow ClassEntities where
    render (Public xs)    = "\npublic:\n"    ++ intercalateEntities xs
    render (Protected xs) = "\nprotected:\n" ++ intercalateEntities xs
    render (Private xs)   = "\nprivate:\n"   ++ intercalateEntities xs


data CppEntities = CppEntities [CppEntity]
                        deriving (Show)

instance CppShow CppEntities where
    render (CppEntities xs) =  intercalateEntities xs                      
            

intercalateEntities :: [CppEntity] -> String  
intercalateEntities xs =  if (null xs) then "" else (intercalate "\n" $ map render xs)  


---------------------------------------------------------
-- Template
--

data TemplateParam = Typename       { getTname :: String }                     |
                     NonType        { getTType :: String, getTname :: String } |
                     TempTempParam  { getTType :: String, getTname :: String }
                        deriving (Show)


instance CppShow TemplateParam where
    render (Typename ts) = "typename " ++ ts
    render (NonType ts value) = ts ++ " " ++ value
    render (TempTempParam ts value) = ts ++ " " ++ value 


newtype Template = Template { getTemplate :: [TemplateParam] }
                    deriving (Show)

instance CppShow Template where
    render (Template xs) = "template <" ++ render (CommaSep xs) ++ ">"


-- ... a template list is also a monoid 
--
    
instance Monoid Template where
    mempty = Template []
    mappend (Template xs) (Template ys) = Template (mappend xs ys)

-- ... utility function
--

(+++) :: Template -> Class -> Class
template +++ (Class name xs) = TClass template name xs
_ +++ (TClass _ _ _)  = error "Template Syntax error"

getFullySpecializedName :: Maybe Template -> String -> String
getFullySpecializedName Nothing ns = ns
getFullySpecializedName (Just (Template xs)) ns = ns ++ "<" ++ (intercalate ", " (map getTname xs) ) ++ ">"


---------------------------------------------------------
-- Cpp Function
--


data Function = Function (Maybe Template) FuncDecl FuncBody 
                    deriving (Show)

instance CppShow Function where
    render (Function templ decl body) = render templ ++ render decl ++ render body 


---------------------------------------------------------
-- Cpp Function Declaration
--

data FuncDecl = FuncDecl [Specifier] (Maybe Type) Identifier (CommaSep ArgType)
                    deriving (Show)

instance CppShow FuncDecl where
    render (FuncDecl sp ret ns args) = "\n" ++ spec ++ ( if (null spec) then "" else " ") ++ render ret ++ 
                                        ( if (null spec && isNothing ret) then "" else "\n" ) ++
                                        ns ++ "("  ++ render(args) ++ ")" 
                                            where spec = render sp

---------------------------------------------------------
-- Cpp Function Body
--
   
data FuncBody = FuncBody [String] | MembFuncBody [String] Qualifier
                    deriving (Show)


instance CppShow FuncBody where
    render (FuncBody xs) =  "\n{" ++ bodyToString xs ++ "}"
                                        where bodyToString [] = []
                                              bodyToString ys = intercalate "\n    " ( "" :ys) ++ "\n"
    render (MembFuncBody xs Unqualified)= render $ FuncBody xs
    render (MembFuncBody xs Constant)   = " const" ++ (render $ FuncBody xs)
    render (MembFuncBody xs Volatile)   = " volatile" ++ (render $ FuncBody xs)
    render (MembFuncBody _  Delete)     = " = delete;"
    render (MembFuncBody _  Default)    = " = default;" 
    render (MembFuncBody _  Pure)       = " = 0;"

