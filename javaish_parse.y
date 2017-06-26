{
module JavaishParser where
import JavaishUtils
import JavaishTokenizer
}

%name javaish
%tokentype { Token }
%error { parseError }

%token
  "class"              { ( TClass,      $$ ) }
  "new"                { ( TNew,        $$ ) }
  "String"             { ( TString,     $$ ) }
  "static"             { ( TStatic,     $$ ) }
  "void"               { ( TVoid,       $$ ) }
  "main"               { ( TMain,       $$ ) }
  "public"             { ( TPublic,     $$ ) }
  "return"             { ( TReturn,     $$ ) }
  "null"               { ( TNull,       $$ ) }
  "extends"            { ( TExtend,     $$ ) }
  "int"                { ( TInt,        $$ ) }
  "boolean"            { ( TBool,       $$ ) }
  "if"                 { ( TIf,         $$ ) }
  "else"               { ( TElse,       $$ ) }
  "true"               { ( TTrue,       $$ ) }
  "false"              { ( TFalse,      $$ ) }
  "this"               { ( TThis,       $$ ) }
  "length"             { ( TLength,     $$ ) }
  "while"              { ( TWhile,      $$ ) }
  "{"                  { ( TLeftBrace,  $$ ) }
  "}"                  { ( TRightBrace, $$ ) }
  ","                  { ( TComma,      $$ ) }
  "["                  { ( TLeftBrack,  $$ ) }
  "]"                  { ( TRightBrack, $$ ) }
  "("                  { ( TLeftParen,  $$ ) }
  ")"                  { ( TRightParen, $$ ) }
  ";"                  { ( TSemiColon,  $$ ) }
  "."                  { ( TPeriod,     $$ ) }
  "!"                  { ( TNot,        $$ ) }
  "="                  { ( TAssign,     $$ ) }
  "System.out.println" { ( TPrint,      $$ ) }
  integer_literal      { ( TIntLiteral _, _ ) }
  ident                { ( TIdent      _, _ ) }
  op                   { ( TOp         _, _ ) }
  comop                { ( TComOp      _, _ ) }
%%

Program :
        MainClass ClassDeclList { Program $1 $2 }
        -- MainClass ClassDeclList { ( $1, $2 ) }

MainClass :
          "class" ident "{" "public" "static" "void" "main" "(" "String" "[" "]" ident ")" "{" Statement "}" "}" { MClass ( tokToIdent $2 ) ( tokToIdent $12 ) $15 $1 }


ClassDeclList :
            ClassDecl ClassDeclList { $1 : $2 }
          |                         { [] }

ClassDecl :
            "class" ident "{" VarDeclList MethodDeclList "}"
                { ClassDecl ( tokToIdent $2 ) "void" $4 $5 $1 }
          | "class" ident "extends" ident "{" VarDeclList MethodDeclList "}"
                { ClassDecl ( tokToIdent $2 ) ( tokToIdent $4 ) $6 $7 $1 }


MethodDeclList :
       MethodDecl MethodDeclList  { $1 : $2 }
     |                            { [] }

MethodDecl :
         "public" Type ident "(" FormalList ")" "{" VarDeclList StatementList "return" Exp ";" "}"
           { MethodDecl $2 ( tokToIdent $3 ) $5 $8 $9 $11 $1 }

VarDeclList :
       VarDeclListNonEmpty   { $1 }
     |                       { [] }

VarDeclListNonEmpty :
       Type ident ";"                     { [ ( $1, tokToIdent $2 ) ] }
     | Type ident ";" VarDeclListNonEmpty { ( $1, tokToIdent $2 ) : $4 }

FormalList :
       Type ident            { [ ( $1, tokToIdent $2 ) ] }
     | Type ident FormalList { ( $1, tokToIdent $2 ) : $3 }

Type :
       "int" "[" "]"  { Type TypeIntArray $1 }
     | "boolean"      { Type TypeBoolean $1 }
     | "int"          { Type TypeInt $1 }
     | ident          { Type ( TypeIdent ( tokToIdent $1 ) ) ( snd $1 ) }

Statement :
      "{" StatementList "}"                       { Stmt ( SList $2 ) $1 }
    | "if" "(" Exp ")" Statement "else" Statement { Stmt ( SIfElse $3 $5 $7 ) $1 }
    | "while" "(" Exp ")" Statement               { Stmt ( SWhile $3 $5 ) $1 }
    | "System.out.println" "(" Exp ")" ";"        { Stmt ( SPrint $3 ) $1 }
    | ident "=" Exp ";"                           { Stmt ( SAssign ( tokToIdent $1 ) $3 ) $2 }
    | ident "[" Exp "]" "=" Exp ";"               { Stmt ( SArrayAssign ( tokToIdent $1 ) $3 $6 ) $5 }

StatementList :
      Statement                { [ $1 ] }
    | StatementList Statement  { $1 ++ [ $2 ] }

Exp :
      Exp op Exp2        { Exp ( ExpOp $1 ( tokToOp $2 ) $3 ) ( snd $2 ) }
    | Exp comop Exp2     { Exp ( ExpComOp $1 ( tokToComOp $2 ) $3 ) ( snd $2 ) }
    | Exp2               { $1 }

Exp2 :
      "!" Exp2           { Exp ( ExpNot $2 ) $1 }
    | Exp3               { $1 }


Exp3 :
      Exp3 "[" Exp "]"                { Exp ( ExpArray $1 $3 ) $2 }
    | Exp3 "." "length"               { Exp ( ExpLength $1 ) $2 }
    | Exp3 "." ident "(" ExpList ")"  { Exp ( ExpFCall $1 ( tokToIdent $3 ) $5 ) $2 }
    | integer_literal                 { Exp ( ExpInt( tokToInt $1 ) ) ( snd $1 ) }
    | "true"                          { Exp ( ExpBool True ) $1 }
    | "false"                         { Exp ( ExpBool False ) $1 }
    | ident                           { Exp ( ExpIdent( tokToIdent $1 ) ) ( snd $1 ) }
    | "this"                          { Exp ( ExpThis ) $1 }
    | "new" "int" "[" Exp "]"         { Exp ( ExpNewInt $4  ) $1 }
    | "new" ident "(" ")"             { Exp ( ExpNewIdent( tokToIdent $2 ) ) $1 }
    | "null"                          { Exp ExpNull $1 }
    | "(" Exp ")"                     { $2 }

ExpList :
          Exp          { [ $1 ] }
        | Exp ExpRest  { $1 : $2 }
        |              { [] }

ExpRest :
     "," Exp      { [ $2 ] }



{
parseError :: [ Token ] -> a
parseError _ = error "Parse error"

-- Syntax tree stuff:

class AstWithPosn a where
  astPosn :: a -> AlexPosn

data Program
    = Program MainClass [ ClassDecl ] AlexPosn
      deriving (Show, Eq)
instance AstWithPosn Program where
  astPosn( Program _ _ p ) = p

data MainClass
    = MClass String String Statement AlexPosn
      deriving (Show, Eq)
instance AstWithPosn MainClass where
  astPosn( MClass _ _ _ p ) = p

data ClassDecl = ClassDecl Ident Ident [ ( Type, Ident ) ] [ MethodDecl ] AlexPosn
  deriving (Show, Eq)
instance AstWithPosn ClassDecl where
  astPosn( ClassDecl _ _ _ _ p ) = p

data MethodDecl
    = MethodDecl Type Ident [ ( Type, Ident ) ] [ ( Type, Ident ) ] [ Statement ] Exp AlexPosn
    deriving (Show, Eq)
instance AstWithPosn MethodDecl where
  astPosn( MethodDecl _ _ _ _ _ _ p ) = p

data TypeOnly =
    TypeIntArray
    | TypeBoolean
    | TypeInt
    | TypeIdent Ident
    deriving (Show, Eq)
data Type = Type TypeOnly AlexPosn
    deriving (Show, Eq)
instance AstWithPosn Type where
  astPosn( Type _ p )= p

data StatementOnly
    = SList [ Statement ]
    | SIfElse Exp Statement Statement
    | SWhile Exp Statement
    | SPrint Exp
    | SAssign Ident Exp
    | SArrayAssign Ident Exp Exp
    | StatementError
    deriving (Show, Eq)
data Statement = Stmt StatementOnly AlexPosn
    deriving (Show, Eq)
instance AstWithPosn Statement where
  astPosn( Stmt _ p )= p

data ExpOnly
    = ExpOp Exp Char Exp
    | ExpComOp Exp Char Exp
    | ExpArray Exp Exp
    | ExpFCall Exp Ident [ Exp ]
    | ExpInt Int
    | ExpNewInt Exp
    | ExpBool Bool
    | ExpIdent Ident
    | ExpNewIdent Ident
    | ExpThis
    | ExpNot Exp
    | ExpLength Exp
    | ExpNull
    | ExpError
    deriving (Show, Eq)
data Exp = Exp ExpOnly AlexPosn
    deriving (Show, Eq)
instance AstWithPosn Exp where
  astPosn( Exp _ p )= p

data Op
     = And
     | LessThan
     | Plus
     | Minus
     | Times
     deriving (Show, Eq)

type Ident = String
type Integer_Literal = Int

data ExpRest
    = ExpRest Exp
    deriving (Show, Eq)
}
