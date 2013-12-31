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

module Cpp.Lang where

import Data.Monoid
import Data.List 
import Data.Maybe

type Identifier = String
type Code = Char
 
---------------------------------------------------------
-- CppShow type class
--
 

class CppShow a where
    render :: a -> String 


-- Existential CppEntity 
--

cpp :: (Show a, CppShow a) => [a] -> [CppEntity]
cpp = map CppEntity  


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
    render xs =  intercalate "\n" $ map render xs


-- (CommaSep a) instance of CppShow
--

instance (CppShow a) => CppShow (CommaSep a) where
    render (CommaSep xs) = intercalate ", " $ map render xs  


---------------------------------------------------------
-- Raw: Raw String for CppShow
--

newtype R = R { getString :: String }
                    deriving (Show)

instance CppShow R where
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
    render = getType 

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
    render (Named t ns) = render t ++ " " ++ ns

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
ctor spec ns qual = function 
                    (FuncDecl spec Nothing ns (CommaSep [])) 
                    (MembFuncBody [] qual)


dtor :: [Specifier] -> Identifier -> Qualifier -> Function
dtor spec ns qual = function 
                    (FuncDecl spec Nothing ('~' : ns) (CommaSep [])) 
                    (MembFuncBody [] qual)


copyCtor :: [Specifier] -> Identifier -> Qualifier -> Function
copyCtor spec ns qual = function 
                        (FuncDecl spec Nothing ns (CommaSep [add_const_lvalue_reference(Named (Type ns) "other")])) 
                        (MembFuncBody [] qual)


moveCtor :: [Specifier] -> Identifier -> Qualifier -> Function
moveCtor spec ns qual = function 
                        (FuncDecl spec Nothing ns (CommaSep [add_rvalue_reference $ Named (Type ns) "other"])) 
                        (MembFuncBody [] qual)


operAssign:: [Specifier] -> Identifier -> Qualifier -> Function
operAssign spec ns qual = function 
                          (FuncDecl spec (Just $ add_lvalue_reference (Type ns)) "operator=" 
                                (CommaSep [add_const_lvalue_reference $ Named (Type ns) "other"])) 
                          (MembFuncBody ["return *this;" ] qual)


operMoveAssign:: [Specifier] -> Identifier -> Qualifier -> Function
operMoveAssign spec ns qual = function
                              (FuncDecl spec (Just $ add_lvalue_reference (Type ns)) "operator=" 
                                    (CommaSep [add_rvalue_reference $ Named (Type ns) "other"])) 
                              (MembFuncBody ["return *this;" ] qual)


---------------------------------------------------------
-- Predefined Cpp free functions
--

_main :: [String] -> Function
_main impl =  function
                (FuncDecl [] (Just $ Type "int") "main" 
                    (CommaSep [Named (Type "int") "argc", Named (Type "char *") "argv[]" ]))
                (FuncBody impl )

operEq :: Maybe Template -> Identifier -> Function
operEq tp xs  = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator==" 
                                (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                            (FuncBody ["/* implementation */" ])        

operNotEq :: Maybe Template -> Identifier -> Function
operNotEq tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator!=" 
                                (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                              (FuncBody ["return !(lhs == rhs);"])

operLt :: Maybe Template -> Identifier -> Function
operLt tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator<"  
                                (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                           (FuncBody ["/* implementation */"])

operLtEq :: Maybe Template -> Identifier -> Function
operLtEq tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator<=" 
                                (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                             (FuncBody ["return !(rhs < lhs);"])

operGt :: Maybe Template -> Identifier -> Function
operGt tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator>"  
                                (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                            (FuncBody ["return rhs < lhs;"])

operGtEq :: Maybe Template -> Identifier -> Function
operGtEq tp xs = Function tp (FuncDecl [Inline] (Just (Type "bool")) "operator>=" 
                                (CommaSep [add_const_lvalue_reference(Named (Type xs) "lhs"), 
                                add_const_lvalue_reference(Named (Type xs) "rhs")])) 
                             (FuncBody ["return !(lsh < rhs);"])

operInsrt :: Maybe Template -> Identifier -> Function
operInsrt tp xs = Function tapp 
                           (FuncDecl [] (Just (Type "typename std::basic_ostream<CharT, Traits> &")) "operator<<" 
                                (CommaSep [Named (Type "std::basic_ostream<CharT,Traits>&") "out", 
                                add_const_lvalue_reference (Named (Type $ getFullySpecializedName tp xs) "that")]))
                           (FuncBody ["return out;"])
                                where tapp =  mappend (Just (Template[Typename "CharT", Typename "Traits"])) tp 

operExtrc :: Maybe Template -> Identifier -> Function
operExtrc tp xs = Function tapp 
                           (FuncDecl [] (Just (Type "typename std::basic_istream<CharT, Traits> &")) "operator>>" 
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
                                    render es ++ 
                                    "\n} // namespace " ++ ns 

---------------------------------------------------------
-- Cpp Class, ClassAccessSpecifier and CppEntities 
--

data Class = Class Identifier BaseSpecifierList [ClassAccessSpecifier] |
             TClass Template Identifier BaseSpecifierList [ClassAccessSpecifier]
                deriving (Show)


instance CppShow Class where
    render (Class ns base es) =  "class " ++ ns ++ render base ++ " {\n" ++ intercalate "\n" (map render es) ++ "\n\n};\n" 
    render (TClass tp ns base es) =  template ++ (if null template then "" else "\n")  ++ "class " ++ ns ++ render base ++ " {\n" ++
                                 intercalate "\n" (map render es) ++ "\n\n};\n" 
                                   where template = render tp


instance CppTemplate Class where
    template +++ (Class name base ns) = TClass template name base ns
    _ +++ (TClass {})  = error "Template Syntax error"


data BaseSpecifierList = NoBaseSpec | BaseSpecList [BaseAccessSpecifier] 
                            deriving (Show)

data BaseAccessSpecifier = BasePublic      [Identifier] |
                           BaseProtected   [Identifier] |
                           BasePrivate     [Identifier]
                                deriving (Show)

data ClassAccessSpecifier = Public      [CppEntity] |
                            Protected   [CppEntity] |
                            Private     [CppEntity]
                                deriving (Show)


class AccessSpecifier a b where
    public    :: (Show a, CppShow a) => [a] -> b
    private   :: (Show a, CppShow a) => [a] -> b
    protected :: (Show a, CppShow a) => [a] -> b


instance AccessSpecifier a ClassAccessSpecifier where
    public    xs = Public    $ map CppEntity xs
    protected xs = Protected $ map CppEntity xs
    private   xs = Private   $ map CppEntity xs


instance AccessSpecifier R BaseAccessSpecifier where
    public    xs = BasePublic    $ map getString xs
    protected xs = BaseProtected $ map getString xs
    private   xs = BasePrivate   $ map getString xs


instance CppShow BaseSpecifierList where
    render (NoBaseSpec)       = ""
    render (BaseSpecList [])  = ""
    render (BaseSpecList xs)  = " : " ++ intercalate ", " (map render xs)


instance CppShow BaseAccessSpecifier where
    render (BasePublic [])    = ""
    render (BasePublic xs)    = "public "    ++ intercalate ", " xs
    render (BaseProtected []) = ""
    render (BaseProtected xs) = "protected " ++ intercalate ", " xs
    render (BasePrivate [])   = ""
    render (BasePrivate xs)   = "private "   ++ intercalate ", " xs


instance CppShow ClassAccessSpecifier where
    render (Public xs)    = "\npublic:\n"    ++ intercalate "\n" (map render xs)
    render (Protected xs) = "\nprotected:\n" ++ intercalate "\n" (map render xs)
    render (Private xs)   = "\nprivate:\n"   ++ intercalate "\n" (map render xs) 


data CppEntities = CppEntities [CppEntity]
                        deriving (Show)

instance CppShow CppEntities where
    render (CppEntities xs) =  intercalate "\n" (map render xs)                      
            

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


class CppTemplate a where
    (+++) :: Template -> a -> a


newtype Template = Template { getTemplate :: [TemplateParam] }
                    deriving (Show)

instance CppShow Template where
    render (Template xs) = "template <" ++ render (CommaSep xs) ++ ">"


instance Monoid Template where
    mempty = Template []
    mappend (Template xs) (Template ys) = Template (mappend xs ys)


getFullySpecializedName :: Maybe Template -> String -> String
getFullySpecializedName Nothing ns = ns
getFullySpecializedName (Just (Template xs)) ns = ns ++ "<" ++ intercalate ", " (map getTname xs) ++ ">"


---------------------------------------------------------
-- Cpp Function
--

data Function = Function (Maybe Template) FuncDecl FuncBody 
                    deriving (Show)


function :: FuncDecl -> FuncBody -> Function
function = Function Nothing

instance CppShow Function where
    render (Function templ decl body) = render templ ++ render decl ++ render body 

instance CppTemplate Function where
    t +++ (Function t' decl body) = Function (Just t `mappend` t') decl body

---------------------------------------------------------
-- Cpp Function Declaration
--

data FuncDecl = FuncDecl [Specifier] (Maybe Type) Identifier (CommaSep ArgType)
                    deriving (Show)

instance CppShow FuncDecl where
    render (FuncDecl sp ret ns args) = "\n" ++ spec ++ (if null spec then "" else " ") ++ render ret ++ 
                                        ( if null spec && isNothing ret then "" else "\n" ) ++
                                        ns ++ "("  ++ render args ++ ")" 
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
    render (MembFuncBody xs Constant)   = " const" ++ render (FuncBody xs)
    render (MembFuncBody xs Volatile)   = " volatile" ++ render (FuncBody xs)
    render (MembFuncBody _  Delete)     = " = delete;"
    render (MembFuncBody _  Default)    = " = default;" 
    render (MembFuncBody _  Pure)       = " = 0;"
