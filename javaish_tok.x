{
module JavaishTokenizer where
import JavaishUtils
}

%wrapper "posn"

$digit    = 0-9            -- digits
$alpha    = [a-zA-Z]       -- alphabetic characters
$graphic  = $printable # $white
$notquote = [$white $printable # \"]

@string  = \" ($graphic # \")* \"
@mstring  = \"{3} ( ( $notquote )* | ( $notquote+ ( \"{1,2} $notquote+ )* ) ) \"{3}

tokens :-
  $white+    ;
  "class"    { addPosn TClass }
  "new"      { addPosn TNew }
  "String"   { addPosn TString }
  "static"   { addPosn TStatic }
  "void"     { addPosn TVoid }
  "main"     { addPosn TMain }
  "return"   { addPosn TReturn }
  "public"   { addPosn TPublic }
  "extends"  { addPosn TExtend }
  "null"     { addPosn TNull }
  "int"      { addPosn TInt }
  "boolean"  { addPosn TBool }
  "if"       { addPosn TIf }
  "else"     { addPosn TElse }
  "true"     { addPosn TTrue }
  "false"    { addPosn TFalse }
  "this"     { addPosn TThis }
  "length"   { addPosn TLength }
  "while"    { addPosn TWhile }
  \-?$digit+ { addPosnS( TIntLiteral . read ) }  -- \s -> TIntLiteral( read s )
  \-?$digit+\.$digit+ { addPosnS( TFloatLiteral . read ) }
  \-?$digit+\.$digit+e\-?$digit+ { addPosnS( TFloatLiteral . read ) }
  "."        { addPosn TPeriod }
  "&&"       { addPosnS( TOp . head ) }
  "!"        { addPosn TNot }
  [\+\-\*\/] { addPosnS( TOp . head ) }
  "<"        { addPosnS( TComOp . head ) }
  "=="       { addPosnS( TComOp . head ) }  -- addPosnS( \s -> TComOp( head s ) )
  "="        { addPosn TAssign }
  ";"        { addPosn TSemiColon }
  "("        { addPosn TLeftParen }
  ")"        { addPosn TRightParen }
  $alpha[$alpha $digit \_ \']* { addPosnS( TIdent ) }
  @string    { addPosnS( TStringLiteral . tail . init ) } -- remove the quotes
  @mstring   { addPosnS( \s -> TStringLiteral( take ( length s - 6 ) ( drop 3 s ) ) ) }
  "{"        { addPosn TLeftBrace }
  "}"        { addPosn TRightBrace }
  ","        { addPosn TComma }
  "["        { addPosn TLeftBrack }
  "]"        { addPosn TRightBrack }
  "System.out.println" { addPosn TPrint }

{
addPosn :: TokenOnly -> AlexPosn -> String -> Token
addPosn t p s = ( t, p )
addPosnS :: ( String -> TokenOnly ) -> AlexPosn -> String -> Token
addPosnS f p s = ( f s, p )

data TokenOnly =
  TLeftBrace |
  TRightBrace |
  TComma |
  TLeftBrack |
  TRightBrack |
  TClass |
  TPublic |
  TString |
  TStatic |
  TVoid |
  TMain |
  TNull |
  TExtend |
  TInt |
  TBool |
  TIf |
  TElse |
  TTrue |
  TFalse |
  TThis |
  TLength |
  TWhile |
  TNew |
  TOp Char |
  TComOp Char |
  TNot |
  TAssign |
  TPeriod |
  TSemiColon |
  TLeftParen |
  TRightParen |
  TIdent String |
  TPrint |
  TIntLiteral Int |
  TFloatLiteral Float |
  TStringLiteral String |
  TReturn
  deriving( Eq, Show )

type Token = ( TokenOnly, AlexPosn )

tokToIdent  ( TIdent         i, _ ) = i
tokToInt    ( TIntLiteral    i, _ ) = i
tokToString ( TStringLiteral s, _ ) = s
tokToOp     ( TOp            o, _ ) = o
tokToComOp  ( TComOp         c, _ ) = c

}
