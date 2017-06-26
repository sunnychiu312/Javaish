--
--
--

import Data.IORef
import Control.Monad
import qualified Data.Set as Set
import qualified Data.Map as Map
import JavaishUtils
import JavaishTokenizer
import JavaishParser

-- table that maps names (variables, fields, formals, etc) to values
type SymbolTable = Map.Map Ident Value

-- definition of an object
--  - class name
--  - reference to field table
--  - (the whole thing wrapped in Maybe, because of Hoare's Billion Dollar Mistake)
data ObjInfo = OI( Maybe ( Ident, IORef SymbolTable ) )
  deriving( Eq )

instance Show ObjInfo where
  show ( OI Nothing ) = "null"
  show ( OI( Just( cname, fields ) ) ) = cname ++ "_obj" -- TODO: address?

nullObj = OI Nothing

-- table that maps method names (within a particular class) to:
--   - list of parameter names
--   - local variable table
--   - method body
--   - return expression
type MethodTable = Map.Map Ident ( [ Ident ], SymbolTable, [ Statement ], Exp )

-- key: class names
-- value:
--  - field table
--  - method table
type ClassTable = Map.Map Ident ( SymbolTable, MethodTable )

data Value
  = VInt    Int
  | VFloat  Float
  | VString String
  | VObj    ObjInfo
  | VBool   Bool
  -- length of the array stored explicitly for performance
  | VArray  Int [ Value ]
  deriving( Eq, Show )

buildClassTable :: Program -> ClassTable
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

interpret :: ClassTable -> Program -> IO ()
interpret classes whole_program = do
  -- each frame of the call stack includes:
  --  - "this"'s class
  --  - "this"'s field table
  --  - local variable table for the current call
  -- callStack :: IORef [ ( Ident, IORef SymbolTable, SymbolTable ) ]
  callStack <- newIORef []

  let
    interp_prog :: Program -> IO ()
    interp_prog ( Program ( MClass name args body _ ) _ _ ) = do
        thisFields <- newIORef Map.empty
        -- TODO: add real args to local state
        writeIORef callStack [ ( name, thisFields, Map.singleton args ( VString "" ) ) ]
        interp_stmt body

    interp_stmt :: Statement -> IO ()
    interp_stmt ( Stmt stmt p ) =
      case stmt of
        -- { <stmts> }
        SList stmts -> forM_ stmts interp_stmt

        -- if <test> then <thenStmt> else <elseStmt>
        SIfElse test thenStmt elseStmt -> do
            VBool testTrue <- interp_exp test
            if testTrue then
              interp_stmt thenStmt
            else
              interp_stmt elseStmt

        -- while( <test> ) { <body> }
        SWhile test body -> do
            VBool testTrue <- interp_exp test
            if testTrue then
              do interp_stmt body
                 interp_stmt ( Stmt stmt p )
            else
              return ()

        -- System.out.println( <toBePrinted> )
        SPrint toBePrinted -> do
            v <- interp_exp toBePrinted
            putStrLn $ show v

        -- <lhs> = <rhs>;
        SAssign lhs rhs -> do
            v <- interp_exp rhs
            ( ( c, fieldsRef, locals ) : s ) <- readIORef callStack
            case Map.lookup lhs locals of
                Just _ ->
                    let locals' = Map.insert lhs v locals in
                    writeIORef callStack ( ( c, fieldsRef, locals' ) : s )
                Nothing -> do
                    fields <- readIORef fieldsRef
                    let fields' = Map.insert lhs v fields
                    writeIORef fieldsRef fields'

        -- <lhs>[ <idx> ] = <rhs>;
        SArrayAssign lhs idx rhs -> do
            VInt i <- interp_exp idx
            v <- interp_exp rhs
            ( ( c, fieldsRef, locals ) : s ) <- readIORef callStack
            case Map.lookup lhs locals of
                Just ( VArray length items ) ->
                    -- if i < 0 or i > length ...???? undefined
                    let ( left, _ : right ) = splitAt i items in
                    let new_items = left ++ ( v : right ) in
                    let locals' = Map.insert lhs ( VArray length new_items ) locals in
                    writeIORef callStack ( ( c, fieldsRef, locals' ) : s )
                Nothing -> do
                    fields <- readIORef fieldsRef
                    let Just ( VArray length items ) = Map.lookup lhs fields
                    let ( left, _ : right ) = splitAt i items
                    let new_items = left ++ ( v : right )
                    let fields' = Map.insert lhs ( VArray length new_items ) fields
                    writeIORef fieldsRef fields'

        StatementError -> undefined


    interp_exp :: Exp -> IO Value
    interp_exp ( Exp e p ) =
      case e of
        -- <eL> <op> <eR>
        ExpOp eL op eR -> do
            vL <- interp_exp eL
            vR <- interp_exp eR
            let
              result = case ( vL, op, vR ) of
                  ( VInt iL, '+', VInt iR ) -> VInt $ iL + iR
                  ( VInt iL, '-', VInt iR ) -> VInt $ iL - iR
                  ( VInt iL, '*', VInt iR ) -> VInt $ iL * iR
                  ( VInt iL, '/', VInt iR ) -> VInt $ quot iL iR
            return result

        -- <eL> <op> <eR>
        ExpComOp eL op eR -> do
            vL <- interp_exp eL
            vR <- interp_exp eR
            let
              result = case ( vL, op, vR ) of
                  ( VInt iL, '<', VInt iR ) -> VBool $ iL < iR
            return result

        -- <arr>[ <idx> ]
        ExpArray arr idx -> do
            VArray len items <- interp_exp arr
            VInt i           <- interp_exp idx
            return $ items !! i

        -- <obj>.<method>( <actuals> )
        ExpFCall obj method actuals -> do
            -- TODO: next line will crash if obj is null
            VObj( OI( Just( cname, fields_ref ) ) ) <- interp_exp obj
            let Just ( _, methodTable ) = Map.lookup cname classes
            let Just ( formal_names, locals, body, ret_exp ) = Map.lookup method methodTable
            actual_vals <- mapM interp_exp actuals
            let params = Map.fromList $ zip formal_names actual_vals
            let locals_plus_params = Map.union locals params
            old_stack <- readIORef callStack
            writeIORef callStack ( ( cname, fields_ref, locals_plus_params ) : old_stack )
            forM_ body interp_stmt
            ret_val <- interp_exp ret_exp
            writeIORef callStack old_stack
            return ret_val

        -- <i>
        ExpInt i -> return $ VInt i

        -- new int[ <length> ]
        ExpNewInt length -> do
            VInt num <- interp_exp length
            return( VArray num ( replicate num ( VInt 0 ) ) )

        -- <b>
        ExpBool b -> return $ VBool b

        -- <v>
        ExpIdent var -> do
            ( ( _, thisFields, locals ) : _ ) <- readIORef callStack
            case Map.lookup var locals of
                Just val -> return val
                Nothing -> do
                    fieldTable <- readIORef thisFields
                    let Just val = Map.lookup var fieldTable
                    return val

        -- new <cname>()
        ExpNewIdent cname -> do
            let Just( fields, _ ) = Map.lookup cname classes
            fieldTable <- newIORef fields
            return $ VObj $ OI $ Just( cname, fieldTable )

        -- "this"
        ExpThis -> do
            ( ( thisCName, thisFields, _ ) : _ ) <- readIORef callStack
            return $ VObj $ OI $ Just( thisCName, thisFields )

        -- !<e>
        ExpNot e -> do
            VBool b <- interp_exp e
            return $ VBool $ not b

        -- <arr>.length
        ExpLength arr -> do
            VArray len _ <- interp_exp arr
            return $ VInt len

        ExpError -> undefined

  interp_prog whole_program

main = do
  inStr <- getContents
  let tokens = alexScanTokens inStr
  let parseTree = javaish( tokens ) ( AlexPn 1 1 1 )
  let classes = buildClassTable parseTree
  interpret classes parseTree
