module JavaishUtils where


mapi :: ( Int -> a -> b ) -> [ a ] -> [ b ]
mapi f l = ( reverse result )
  where ( _, result ) = foldl helper ( 0, [] ) l
        -- helper :: ( Int, [ b ] ) -> a -> ( Int, [ b ] )
        helper ( i, lo ) item = ( i + 1, ( f i item ) : lo )
