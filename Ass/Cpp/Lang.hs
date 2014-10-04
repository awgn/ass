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

module Ass.Cpp.Lang where

import Data.Monoid
import Data.List

type Identifier = String
type Code = Char

---------------------------------------------------------
-- CppShow type class
--


class CppShow a where
    render :: a -> String


-- Existential CppEntity
--

cpp :: (CppShow a) => [a] -> [CppEntity]
cpp = map CppEntity


data CppEntity = forall a. (CppShow a) => CppEntity a

instance  CppShow CppEntity where
    render (CppEntity xs) = render xs

-- CommaSep list
--

newtype CommaSep a = CommaSep { getCommaSep :: [a] }

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


class ArgShow a where
    argRender :: a -> String

instance ArgShow ArgType where
    argRender x = "(" ++ render x ++ ")"

instance ArgShow () where
    argRender () = "()"

instance (CppShow a, CppShow b) => ArgShow (a,b) where
    argRender (x,y)  = "(" ++ render x ++ ", " ++ render y ++ ")"

instance (CppShow a, CppShow b, CppShow c) => ArgShow (a,b, c) where
    argRender (x,y,z) = "(" ++ render x ++ ", " ++ render y ++ ", " ++ render z ++ ")"


---------------------------------------------------------
-- Raw: Raw String for CppShow
--

newtype R = R { getString :: String }

instance CppShow R where
    render = getString

---------------------------------------------------------
-- Cpp Specifier and Qualifier:
-- a very approximate rendering for (member) functions...
--

data Specifier = Inline | Static | Virtual | Constexpr

class CppSpecifier a where
    inline :: a -> a
    static :: a -> a
    virtual   :: a -> a
    constexpr :: a -> a

instance CppShow Specifier where
    render Inline    = "inline"
    render Static    = "static"
    render Virtual   = "virtual"
    render Constexpr = "constexpr"


data Qualifier = Unqualified | Constant | Volatile | Delete | Default | Pure

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

instance CppShow Using where
    render (Using member) = "using " ++ member ++ ";\n";
    render (UsingNamespace name) = "using namespace " ++ name ++ ";\n";


---------------------------------------------------------
-- Include directive

data Include = Include FilePath | RelativeInclude FilePath

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

noType :: Type
noType = Type ""

bool, int, unsigned_int, short, unsigned_short, char, unsigned_char, double, string :: Type

bool           = Type "bool"
int            = Type "int"
unsigned_int   = Type "unsigned int"
short          = Type "short"
unsigned_short = Type "unsigned short"
char           = Type "char"
unsigned_char  = Type "unsigned char"
double         = Type "double"
string         = Type "std::string"

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


data ArgType = UnnamedArg Type | Arg Type Identifier

instance CppShow ArgType where
    render (UnnamedArg t)  = render t
    render (Arg t ns) = render t ++ " " ++ ns

instance CppType ArgType where
    getType              (UnnamedArg t) = getType t
    getType              (Arg t _) = getType t
    add_cv               (UnnamedArg t) = UnnamedArg (add_cv t)
    add_cv               (Arg t n) = Arg (add_cv t) n
    add_const            (UnnamedArg t) = UnnamedArg (add_const t)
    add_const            (Arg t n) = Arg (add_const t) n
    add_volatile         (UnnamedArg t) = UnnamedArg (add_volatile t)
    add_volatile         (Arg t n) = Arg (add_volatile t) n
    add_lvalue_reference (UnnamedArg t) = UnnamedArg (add_lvalue_reference t)
    add_lvalue_reference (Arg t n) = Arg (add_lvalue_reference t) n
    add_rvalue_reference (UnnamedArg t) = UnnamedArg (add_rvalue_reference t)
    add_rvalue_reference (Arg t n) = Arg (add_rvalue_reference t) n
    add_pointer          (UnnamedArg t) = UnnamedArg (add_pointer t)
    add_pointer          (Arg t n) = Arg (add_pointer t) n


add_const_lvalue_reference :: (CppType a) => a -> a
add_const_lvalue_reference = add_lvalue_reference . add_const

---------------------------------------------------------
-- Cpp Namespace
--

data Namespace = Namespace Identifier [CppEntity]

instance CppShow Namespace where
    render (Namespace ns es)   = "namespace " ++ ns ++ " {\n" ++
                                    render es ++
                                    "\n} // namespace " ++ ns

---------------------------------------------------------
-- Cpp Class, ClassAccessSpecifier and CppEntities
--

data Class = Class Identifier BaseSpecifierList [ClassAccessSpecifier] |
             TClass Template Identifier BaseSpecifierList [ClassAccessSpecifier]


instance CppShow Class where
    render (Class name base es) =  "class " ++ name ++ render base ++ " {\n" ++ intercalate "\n" (map render es) ++ "\n\n};\n"
    render (TClass tp name base es) =  template ++ (if null template then "" else "\n")  ++ "class " ++ name ++ render base ++ " {\n" ++
                                 intercalate "\n" (map render es) ++ "\n\n};\n"
                                   where template = render tp


instance CppTemplate Class where
    template +++ (Class name base bname) = TClass template name base bname
    _ +++ (TClass {})  = error "Template Syntax error"


data BaseSpecifierList = NoBaseSpec | BaseSpecList [BaseAccessSpecifier]

data BaseAccessSpecifier = BasePublic    [Identifier] |
                           BaseProtected [Identifier] |
                           BasePrivate   [Identifier]

data ClassAccessSpecifier = Public    [CppEntity] |
                            Protected [CppEntity] |
                            Private   [CppEntity]


class AccessSpecifier a b where
    public    :: (CppShow a) => [a] -> b
    private   :: (CppShow a) => [a] -> b
    protected :: (CppShow a) => [a] -> b


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

instance CppShow CppEntities where
    render (CppEntities xs) =  intercalate "\n" (map render xs)


---------------------------------------------------------
-- Template
--

data TemplateParam = Typename       { getTname :: String }                     |
                     NonType        { getTType :: String, getTname :: String } |
                     TempTempParam  { getTType :: String, getTname :: String }


instance CppShow TemplateParam where
    render (Typename ts) = "typename " ++ ts
    render (NonType ts value) = ts ++ " " ++ value
    render (TempTempParam ts value) = ts ++ " " ++ value


class CppTemplate a where
    (+++) :: Template -> a -> a


newtype Template = Template { getTemplate :: [TemplateParam] }

instance CppShow Template where
    render (Template xs) = "template <" ++ render (CommaSep xs) ++ ">"


instance Monoid Template where
    mempty = Template []
    mappend (Template xs) (Template ys) = Template (mappend xs ys)


getFullySpecializedName :: Maybe Template -> String -> String
getFullySpecializedName Nothing name = name
getFullySpecializedName (Just (Template xs)) name = name ++ "<" ++ intercalate ", " (map getTname xs) ++ ">"


---------------------------------------------------------
-- Cpp Function
--

data Function = Function (Maybe Template) FuncDecl FuncBody


function :: FuncDecl -> FuncBody -> Function
function = Function Nothing

instance CppSpecifier Function where
    inline (Function t d b)    = Function t (inline d) b
    static (Function t d b)    = Function t (static d) b
    virtual (Function t d b)   = Function t (virtual d) b
    constexpr (Function t d b) = Function t (virtual d) b


instance CppShow Function where
    render (Function templ decl body) = render templ ++ render decl ++ render body

instance CppTemplate Function where
    t +++ (Function t' decl body) = Function (Just t `mappend` t') decl body


---------------------------------------------------------
-- Cpp Function Declaration
--

data FuncDecl = forall args . (ArgShow args) => FuncDecl [Specifier] Type Identifier args


instance CppSpecifier FuncDecl where
    inline (FuncDecl xs t id' arg)    = FuncDecl (Inline : xs) t id' arg
    static (FuncDecl xs t id' arg)    = FuncDecl (Static : xs) t id' arg
    virtual (FuncDecl xs t id' arg)   = FuncDecl (Virtual : xs) t id' arg
    constexpr (FuncDecl xs t id' arg) = FuncDecl (Constexpr : xs) t id' arg


instance CppShow FuncDecl where
    render (FuncDecl sp ret name args) = "\n" ++ spec ++ (if null spec then "" else " ") ++ render ret ++
                                        ( if null spec && null (getType ret) then "" else "\n" ) ++
                                        name ++ argRender args
                                            where spec = render sp

---------------------------------------------------------
-- Cpp Function Body
--

data FuncBody = Body [String] | MembBody [String] Qualifier


instance CppShow FuncBody where
    render (Body xs) =  "\n{" ++ bodyToString xs ++ "}"
                                        where bodyToString [] = []
                                              bodyToString ys = intercalate "\n    " ( "" :ys) ++ "\n"
    render (MembBody xs Unqualified)= render $ Body xs
    render (MembBody xs Constant)   = " const" ++ render (Body xs)
    render (MembBody xs Volatile)   = " volatile" ++ render (Body xs)
    render (MembBody _  Delete)     = " = delete;"
    render (MembBody _  Default)    = " = default;"
    render (MembBody _  Pure)       = " = 0;"

---------------------------------------------------------
-- Predefined Cpp member Functions
--

ctor :: Identifier -> Qualifier -> Function
ctor name qual =
    function
        (FuncDecl [] noType name ())
        (MembBody [] qual)


dtor :: Identifier -> Qualifier -> Function
dtor name qual =
    function (FuncDecl [] noType ('~' : name) ())
             (MembBody [] qual)


copyCtor :: Identifier -> Qualifier -> Function
copyCtor name qual =
    function (FuncDecl [] noType name (add_const_lvalue_reference $ Arg (Type name) "other"))
             (MembBody [] qual)


moveCtor :: Identifier -> Qualifier -> Function
moveCtor name qual =
    function (FuncDecl [] noType name (add_rvalue_reference $ Arg (Type name) "other"))
             (MembBody [] qual)


operAssign:: Identifier -> Qualifier -> Function
operAssign name qual =
    function (FuncDecl [] (add_lvalue_reference (Type name))
                          "operator=" (add_const_lvalue_reference $ Arg (Type name) "other"))
             (MembBody ["return *this;" ] qual)


operMoveAssign:: Identifier -> Qualifier -> Function
operMoveAssign name qual =
    function (FuncDecl [] (add_lvalue_reference (Type name))
                           "operator=" (add_rvalue_reference $ Arg (Type name) "other"))
             (MembBody ["return *this;" ] qual)


---------------------------------------------------------
-- Predefined Cpp free functions
--

_main :: [String] -> Function
_main impl =  function
    (FuncDecl [] int "main" (Arg int "argc", Arg (add_pointer char) "argv[]"))
    (Body impl)

operEq :: Maybe Template -> Identifier -> Function
operEq tp xs  = Function tp
    (FuncDecl [Inline] bool "operator==" (add_const_lvalue_reference(Arg (Type xs) "lhs"),
                                          add_const_lvalue_reference(Arg (Type xs) "rhs")))
    (Body ["/* implementation */" ])

operNotEq :: Maybe Template -> Identifier -> Function
operNotEq tp xs = Function tp
    (FuncDecl [Inline] bool "operator!=" (add_const_lvalue_reference(Arg (Type xs) "lhs"),
                                          add_const_lvalue_reference(Arg (Type xs) "rhs")))
    (Body ["return !(lhs == rhs);"])

operLt :: Maybe Template -> Identifier -> Function
operLt tp xs = Function tp
    (FuncDecl [Inline] bool "operator<"  (add_const_lvalue_reference(Arg (Type xs) "lhs"),
                                          add_const_lvalue_reference(Arg (Type xs) "rhs")))
    (Body ["/* implementation */"])

operLtEq :: Maybe Template -> Identifier -> Function
operLtEq tp xs = Function tp
    (FuncDecl [Inline] bool "operator<=" (add_const_lvalue_reference(Arg (Type xs) "lhs"),
                                          add_const_lvalue_reference(Arg (Type xs) "rhs")))
    (Body ["return !(rhs < lhs);"])

operGt :: Maybe Template -> Identifier -> Function
operGt tp xs = Function tp
    (FuncDecl [Inline] bool "operator>"  (add_const_lvalue_reference(Arg (Type xs) "lhs"),
                                          add_const_lvalue_reference(Arg (Type xs) "rhs")))
    (Body ["return rhs < lhs;"])

operGtEq :: Maybe Template -> Identifier -> Function
operGtEq tp xs = Function tp
    (FuncDecl [Inline] bool "operator>=" (add_const_lvalue_reference(Arg (Type xs) "lhs"),
                                          add_const_lvalue_reference(Arg (Type xs) "rhs")))
    (Body ["return !(lhs < rhs);"])

operInsrt :: Maybe Template -> Identifier -> Function
operInsrt tp xs = Function template
    (FuncDecl [] (Type "typename std::basic_ostream<CharT, Traits> &")
                 "operator<<" (Arg (Type "std::basic_ostream<CharT,Traits>&") "out",
                               add_const_lvalue_reference (Arg (Type $ getFullySpecializedName tp xs) "that")))
    (Body ["return out;"])
        where template =  mappend (Just (Template[Typename "CharT", Typename "Traits"])) tp

operExtrc :: Maybe Template -> Identifier -> Function
operExtrc tp xs = Function template
    (FuncDecl [] (Type "typename std::basic_istream<CharT, Traits> &")
                  "operator>>" (Arg (Type "std::basic_istream<CharT,Traits>&") "in",
                                add_lvalue_reference $ Arg (Type $ getFullySpecializedName tp xs) "that"))
    (Body ["return in;"])
        where template = mappend (Just (Template[Typename "CharT", Typename "Traits"])) tp


