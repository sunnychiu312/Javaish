--
--
--

module TypeChecker where

import qualified Data.Set as Set
import qualified Data.Map as Map
import JavaishUtils
import JavaishTokenizer
import JavaishParser

-- symbol table. Map from variable/field/param to its type
type SymbolTable = Map.Map Ident TypeOnly
-- method table. Map from method name to tuple ( return type, parameter types )
type MethodTable = Map.Map Ident ( TypeOnly, [ TypeOnly ] )
-- class table. Map from class name field table and method table for that class
type ClassTable  = Map.Map Ident ( SymbolTable, MethodTable )

class_info :: Program -> ClassTable
class_info( Program ( MClass main_name _ _ _ ) classes _ ) =
  foldl analyzeClass main_class_table classes
  where
    main_class_table = Map.singleton main_name ( Map.empty, Map.empty )
    analyzeClass :: ClassTable -> ClassDecl -> ClassTable
    analyzeClass cinfos ( ClassDecl name _ fields methods _ ) =
      Map.insert name ( field_table, method_table ) cinfos
      where
        analyzeField :: SymbolTable -> ( Type, Ident ) -> SymbolTable
        analyzeField table ( Type ty _, id ) = Map.insert id ty table
        field_table = foldl analyzeField Map.empty fields

        analyzeMethod :: MethodTable -> MethodDecl -> MethodTable
        analyzeMethod table ( MethodDecl ( Type retType _ ) name formals _ _ _ _ ) =
          Map.insert name ( retType, formal_types ) table
          where
            formal_types = map ( \( Type t _, _ ) -> t ) formals
        method_table = foldl analyzeMethod Map.empty methods

typecheck_prog :: ClassTable -> Program -> Bool
typecheck_prog cinfo ( Program m classes p ) =
  typecheck_main cinfo m && all ( typecheck_classdecl cinfo ) classes

typecheck_main :: ClassTable -> MainClass -> Bool
typecheck_main cinfo ( MClass name args stmt p ) =
  check_stmt cinfo name {- FIXME: -}Map.empty stmt

typecheck_classdecl :: ClassTable -> ClassDecl -> Bool
typecheck_classdecl cinfo ( ClassDecl name extends vdecls methods p ) =
  case Map.lookup name cinfo of
    Just ( field_table, _ ) ->
      all ( typecheck_mdecl cinfo field_table name ) methods
    Nothing -> False

typecheck_mdecl :: ClassTable -> SymbolTable -> Ident -> MethodDecl -> Bool
typecheck_mdecl cinfo field_table cname
                 ( MethodDecl ( Type rType pt ) name formals locals body rExp p ) =
  case typecheck_exp cinfo cname symtab rExp of
    Just rExpType -> body_check && rType == rExpType
    Nothing -> False
  where
    body_check = all ( check_stmt cinfo cname symtab ) body
    symtab = foldl analyzeVar field_table ( formals ++ locals )
    analyzeVar m ( Type ty _, id ) = Map.insert id ty m

check_stmt :: ClassTable -> Ident -> SymbolTable -> Statement -> Bool
check_stmt cinfo cname symtab ( Stmt stmt p ) = case stmt of
  ( SList stmts ) -> all check_stmt_lcl stmts
  ( SIfElse e st se ) ->
    case typecheck_exp_lcl e of
      Just( TypeBoolean ) -> all check_stmt_lcl [ st, se ]
      _ -> False
  ( SWhile e body ) ->
    case typecheck_exp_lcl e of
      Just( TypeBoolean ) -> check_stmt_lcl body
      _ -> False
  ( SPrint e ) ->
    case typecheck_exp_lcl e of
      Just _ -> True
      _ -> False
  ( SAssign lhs rhs ) ->
    case typecheck_exp_lcl rhs of
      Just _ -> True
      _ -> False
  ( SArrayAssign lhs idx rhs ) ->
    case ( typecheck_exp_lcl idx, typecheck_exp_lcl rhs ) of
      ( Just TypeInt, Just _ ) -> True
      _ -> False
  ( StatementError ) -> False
  where
    check_stmt_lcl = check_stmt cinfo cname symtab
    typecheck_exp_lcl  = typecheck_exp  cinfo cname symtab

typecheck_exp :: ClassTable -> Ident -> SymbolTable -> Exp          -> Maybe TypeOnly
typecheck_exp    cinfo         cname    symtab          ( Exp exp p ) = case exp of
  ( ExpBool _ ) -> Just( TypeBoolean )
  ( ExpInt _ )  -> Just( TypeInt )
  ( ExpNewInt e ) ->
    case typecheck_exp_lcl e of
      Just TypeInt -> Just( TypeIntArray )
      _ -> Nothing
  ( ExpArray e1 e2 ) ->
    case typecheck_exp_lcl e1 of
      Just TypeIntArray ->
        case typecheck_exp_lcl e2 of
          Just TypeInt -> Just( TypeInt )
          _ -> Nothing
      _ -> Nothing
  ( ExpNot e ) ->
    case typecheck_exp_lcl e of
      Just TypeBoolean -> Just( TypeBoolean )
      _ -> Nothing
  ( ExpOp e1 c e2 ) ->
    case ( t1, t2 ) of
      ( Just TypeInt, Just TypeInt ) ->
        if elem c "+-/*" then
          Just TypeInt
        else
          Nothing
      ( Just TypeBoolean, Just TypeBoolean ) ->
        if elem c "&" then
          Just TypeBoolean
        else
          Nothing
      _ -> Nothing
    where
      t1 = typecheck_exp_lcl e1
      t2 = typecheck_exp_lcl e2
  ( ExpNewIdent cname ) ->
    if Map.member cname cinfo then
      Just( TypeIdent cname )
    else
      Nothing
  ( ExpComOp e1 c e2 ) ->
    do
      jt1 <- typecheck_exp_lcl e1
      jt2 <- typecheck_exp_lcl e2
      -- putStrLn "foo"
      case ( jt1, jt2 ) of
           ( TypeInt, TypeInt ) ->
             if elem c "<=" then
               Just TypeBoolean
             else
               Nothing
           _ ->
             if jt1 == jt2 && elem c "=" then
               Just TypeBoolean
             else
               Nothing
  ( ExpFCall obj m ps ) -> Nothing
  ( ExpIdent v ) -> Map.lookup v symtab
  ( ExpThis ) -> Just( TypeIdent cname )
  ( ExpLength e ) -> Nothing
  ( ExpError ) -> Nothing
  where
    typecheck_exp_lcl  = typecheck_exp cinfo cname symtab

foo = do
  inStr <- getContents
  let tokens = alexScanTokens inStr
  let parseTree = javaish( tokens ) ( AlexPn 1 1 1 )
  if typecheck_prog ( class_info parseTree ) parseTree then
    putStrLn "TypeCheck OK!!!"
  else
    putStrLn "TypeCheck Failed"
