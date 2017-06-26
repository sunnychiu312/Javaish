--
--
--

import qualified Data.Map as Map
import JavaishUtils
import JavaishTokenizer
import JavaishParser
import qualified TypeChecker as TC

type SymbolTable = Map.Map Ident TypeOnly

swap ( x, y ) = ( y, x )

buildClassTable :: Program -> String -- ClassTable
buildClassTable p = ""
{-
buildClassTable( Program ( MClass main_name _ _ _ ) classes _ ) =
  foldl analyzeClass main_class_table classes
  where
    main_class_table = Map.singleton main_name ( Map.empty, Map.empty )
    analyzeClass :: ClassTable -> ClassDecl -> ClassTable
    analyzeClass cinfos ( ClassDecl name _ fields methods _ ) =
      Map.insert name ( field_table, method_table ) cinfos
      where
        analyzeField :: SymbolTable -> ( Type, Ident ) -> SymbolTable
        analyzeField table ( Type ty _, id ) = Map.insert id ( defaultVal ty ) table
        field_table = foldl analyzeField Map.empty fields

        analyzeMethod :: MethodTable -> MethodDecl -> MethodTable
        analyzeMethod table ( MethodDecl _ name formals locals body retExp _ ) =
          Map.insert name ( formal_names, local_table, body, retExp ) table
          where
            formal_names = map snd formals
            local_table = foldl analyzeField Map.empty locals
        method_table = foldl analyzeMethod Map.empty methods

        defaultVal t = case t of
          TypeIntArray -> VArray 0 []
          TypeBoolean  -> VBool False
          TypeInt      -> VInt 0
          TypeIdent _  -> VObj nullObj
-}

compileProg :: Program -> String
compileProg ( Program ( MClass name args body p1 ) class_decls p2 ) =
  header ++ fdecls ++ fdefns ++ main_defn
  where
    header = "#include <stdlib.h>\n#include <runtime_system.h>\n"
    ( fdecls, fdefns, inits ) = foldl doCDecl ( "", "", "" ) class_decls
    doCDecl ( c, f, i ) cd = ( c ++ cn, f ++ fn, i ++ ins )
      where ( cn, fn, ins ) = compileCDecl cd
    main_defn = "void __javaish_main( char ** " ++ args ++ " ) {\n"
      ++ inits ++ compileStmt Map.empty body ++ "\n}\n"


compileType :: Type -> String
compileType ( Type t p ) = case t of
    TypeIntArray         -> "__javaish_int_array_t"
    TypeBoolean          -> "int"
    TypeInt              -> "int"
    TypeIdent class_name -> "struct __javaish_obj_" ++ class_name ++ "_s *"

defaultValue :: Type -> String
defaultValue ( Type t p ) = case t of
    TypeIntArray         -> "( ( __javaish_int_array_t ) 0 )"
    TypeBoolean          -> "0"
    TypeInt              -> "0"
    TypeIdent class_name -> "( ( " ++ compileType ( Type t p ) ++ " ) 0 )"

compileCDecl :: ClassDecl -> ( String, String, String )
compileCDecl ( ClassDecl name extends fields methods p ) =
  ( decls, defns, method_ref_inits )
  where
    obj_type = compileType( Type ( TypeIdent name ) p )

    obj_type_decls = "struct __javaish_obj_" ++ name ++ "_s;\n"
      ++ "struct __javaish_class_" ++ name ++ "_s {\n"
      ++ method_ref_decls
      ++ "} __javaish_class_desc_" ++ name ++ ";\n"
      ++ "struct __javaish_obj_" ++ name ++ "_s {\n"
      ++ "    struct __javaish_class_" ++ name ++ "_s *class;\n"
      ++ "    struct {\n" ++ field_decls ++ "    } fields;\n};\n"
    field_decls = foldl declField "" fields --  fields = [( Type, Ident ) ]
    declField :: String -> ( Type, Ident ) -> String
    declField s ( t, n ) = s ++ "    " ++ compileType t ++ " " ++ n ++ ";\n"
    ( method_ref_decls, method_ref_inits ) = foldl declMethodRef ( "", "" ) methods
    declMethodRef :: ( String, String ) -> MethodDecl -> ( String, String )
    declMethodRef ( d, i ) ( MethodDecl retType mname formals _ _ _ p ) =
      ( d ++ "    " ++ compileType retType ++ " (*" ++ mname ++ ")( "
        ++ obj_type ++ formal_decls ++ ");\n",
        i ++ "    __javaish_class_desc_" ++ name ++ "." ++ mname
        ++ " = __javaish_fn_" ++ name ++ "_" ++ mname ++ ";\n" )
      where
        formal_decls = case formals of
          [] -> ""
          _ -> foldl (\s ( t, _ ) -> s ++ ", " ++ compileType t ) "" formals

    new_decl = obj_type ++ " __javaish_new_" ++ name ++ "( void );\n"
    new_defn = obj_type ++ " __javaish_new_" ++ name ++ "( void ) {\n"
      ++ "    " ++ obj_type ++ " o = (" ++ obj_type ++ " )malloc( sizeof( o[0] ) );\n"
      ++ "    o->class = &__javaish_class_desc_" ++ name ++ ";\n"
      ++ field_inits ++ "    return o;\n}\n"
    field_inits = foldl initField "" fields
    initField :: String -> ( Type, Ident ) -> String
    initField s ( t, n ) = s ++ "    " ++ compileType t ++ " " ++ n ++ " = "
      ++ defaultValue t ++ ";\n"

    fdecls = foldl (\s m -> s ++ compileFDecl name m ) "" methods
    fdefns = foldl (\s m -> s ++ compileFDefn name m ) "" methods

    decls = obj_type_decls ++ new_decl ++ fdecls
    defns = new_defn ++ fdefns

compileFDecl :: String -> MethodDecl -> String
compileFDecl class_name ( MethodDecl ret_type name formals _ _ _ p ) =
  compileType ret_type
  ++" __javaish_fn_"++class_name++"_"++name
  ++"( struct __javaish_obj_"++class_name++"_s *this" ++ params ++");\n"
  where
    params = case formals of
      [] -> ""
      _ -> foldl (\s ( t, v ) -> ", " ++ compileType t ++ " " ++ v ) "" formals

compileFDefn :: String -> MethodDecl -> String
compileFDefn class_name ( MethodDecl ret_type name formals locals body ret_exp p ) =
  compileType ret_type
  ++" __javaish_fn_"++class_name++"_"++name
  ++"( struct __javaish_obj_"++class_name++"_s *this" ++ params ++"){\n"
  ++foldl ( \str ( t, v ) -> str ++ compileType t ++ " " ++ v ++ ";\n" ) "" locals
  ++foldl ( \str stmt -> str ++ compileStmt lmap stmt ) "" body
  ++"return "++compileExp lmap ret_exp++";\n}\n"
  where
    params = case formals of
      [] -> ""
      _ -> foldl (\s ( t, v ) -> ", " ++ compileType t ++ " " ++ v ) "" formals
    lmap = Map.fromList $ map ( \( Type t p, v ) -> ( v, t ) ) ( formals ++ locals )

compileStmt :: SymbolTable -> Statement -> String
compileStmt locals ( Stmt stmt ( AlexPn line col byte ) ) =
  let compStmt = compileStmt locals in
  let compExp = compileExp locals in
  case stmt of
    -- { <stmts> }
    SList stmts ->
      -- "#line "++show line++" "++filename++"\n"++
      "{\n" ++ foldl ( \str stmt -> str ++ compStmt stmt ) "" stmts ++ "}\n"

    -- if <test> then <thenStmt> else <elseStmt>
    SIfElse test thenStmt elseStmt ->
      "if ("++compExp test++") {"++compStmt thenStmt++" } else { "
        ++compStmt elseStmt ++ "}"

    -- while( <test> ) { <body> }
    SWhile test body ->
      "while ("++compExp test++") { "++compStmt body++"}"

    -- System.out.println( <toBePrinted> )
    SPrint toBePrinted -> "/* PRINT */;"
      where
        v = compExp toBePrinted

    -- <lhs> = <rhs>;
    SAssign lhs rhs_exp ->
      if Map.member lhs locals then
        "    " ++ lhs ++ " = " ++ rhs ++ ";\n"
      else
        "    this->fields." ++ lhs ++ " = " ++ rhs ++ ";\n"
      where rhs = compExp rhs_exp

    -- <lhs>[ <idx> ] = <rhs>;
    SArrayAssign lhs idx rhs -> "ARRAYASSIGN"

    StatementError -> undefined

compileExp :: SymbolTable -> Exp -> String
compileExp locals ( Exp e p ) =
  let compExp = compileExp locals in
  case e of
    -- <eL> <op> <eR>
    ExpOp eL op eR ->
      "(" ++ compExp eL ++ ")" ++ [ op ] ++ "(" ++ compExp eR ++ ")"

    -- <eL> <comop> <eR>
    ExpComOp eL op eR ->
      "(" ++ compExp eL ++ ")" ++ [ op ] ++ "(" ++ compExp eR ++ ")"

    -- <arr_exp> [ <idx_exp> ]
    ExpArray arr_exp idx_exp ->
      "(" ++ compExp arr_exp ++ ")->items[" ++ compExp idx_exp ++ "]"

    -- <obj_exp> . <meth_name> ( <actual_exps> )
    ExpFCall obj_exp meth_name actual_exps ->
      "( "++obj++" )->class->" ++ meth_name ++ "("++ actuals ++")"
      where
        actuals = case actual_exps of
          []  -> obj
          _ -> obj ++ foldl (\ s e -> s ++ ", " ++ compExp e ) "" actual_exps
        obj = compExp obj_exp

    -- <i>
    ExpInt i -> show i

    -- new int [ <len_exp> ]
    ExpNewInt len_exp ->
      "__javaish_new_array(" ++ compExp len_exp ++ ")"

    -- <b>
    ExpBool b -> if b then "1" else "0"

    -- <var>
    ExpIdent var ->
      if Map.member var locals then
        var
      else
        "this->fields." ++ var

    -- new <class_name>()
    ExpNewIdent class_name ->
      "__javaish_new_" ++ class_name ++ "()"

    -- "this"
    ExpThis -> "this"

    -- "null"
    ExpNull -> "0"

    -- ! <e>
    ExpNot e -> "!(" ++ compExp e ++ ")"

    -- <e> .length
    ExpLength e ->
      "(" ++ compExp e ++ ")->length";

    -- error?
    ExpError -> undefined

main = do
  inStr <- getContents
  let tokens = alexScanTokens inStr
  let parseTree = javaish( tokens ) ( AlexPn 1 1 1 )
  if TC.typecheck_prog ( TC.class_info parseTree ) parseTree then
    do
      let classes = buildClassTable parseTree
      let c_version = compileProg parseTree
      putStrLn c_version
  else
    putStrLn "TypeCheck Failed"
