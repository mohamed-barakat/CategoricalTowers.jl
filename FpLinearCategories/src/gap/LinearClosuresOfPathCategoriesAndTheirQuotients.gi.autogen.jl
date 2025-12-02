# SPDX-License-Identifier: GPL-2.0-or-later
# FpLinearCategories: Finitely presented linear categories by generating quivers and relations
#
# Implementations
#

@BindGlobal( "LINEAR_CLOSURE_OF_PATH_CATEGORIES_OR_THEIR_QUOTIENTS",
  @FunctionWithNamedArguments(
  [
    [ "admissible_order", fail ],
    [ "colors", fail ],
  ],
  function ( CAP_NAMED_ARGUMENTS, rows, C )
    local k, sorting_func, order, kC;
    
    k = CommutativeRingOfLinearCategory( rows );
    
    if (colors == fail)
        
        colors = @rec( coeff = "", other = "", reset = "" );
        
    elseif (colors == true)
        
        colors = @rec( coeff = "\033[35m", other = "\033[31m", reset = "\033[0m" );
        
    end;
    
    if (admissible_order == fail)
        
        if (IsPathCategory( C ))
            admissible_order = C.admissible_order;
        elseif (IsQuotientCategory( C ))
            admissible_order = AmbientCategory( C ).admissible_order;
        else
            Error( "the category `C` is neither IsPathCategory nor IsQuotientCategory" );
        end;
        
    else
        
        @Assert( 0, admissible_order in [ "Dp", "dp" ] );
        
    end;
    
    order = admissible_order;
    
    if (IsPathCategory( C ))
        
        sorting_func = ( mor_1, mor_2 ) -> IsDescendingForMorphisms( C, mor_1, mor_2, order );
        
    else
        
        sorting_func = ( mor_1, mor_2 ) -> IsDescendingForMorphisms( AmbientCategory( C ), CanonicalRepresentative( mor_1 ), CanonicalRepresentative( mor_2 ), order );
        
    end;
    
    kC = LINEAR_CLOSURE_CONSTRUCTOR_USING_CategoryOfRows( rows, C, sorting_func; FinalizeCategory = false ); # every morphism starts by its maximum monomial
    
    SetIsObjectFiniteCategory( kC, true );
    
    kC.admissible_order = order;
    
    kC.colors = colors;
    
    kC.Name = @Concatenation( RingName( k ), "-", kC.Name );
    
    SetUnderlyingQuiver( kC, UnderlyingQuiver( C ) );
    
    AddSetOfObjectsOfCategory( kC,
      function( kC )
        local C;
        
        C = UnderlyingCategory( kC );
        
        return List( SetOfObjects( C ), o -> ObjectConstructor( kC, o ) );
        
    end );
    
    AddSetOfGeneratingMorphismsOfCategory( kC,
      function( kC )
        local C;
        
        C = UnderlyingCategory( kC );
        
        return List( SetOfGeneratingMorphisms( C ), m ->
                     MorphismConstructor( kC,
                             SetOfObjects( kC )[ObjectIndex( Source( m ) )],
                             PairGAP( [ One( UnderlyingRing( kC ) ) ], [ m ] ),
                             SetOfObjects( kC )[ObjectIndex( Target( m ) )] ) );
        
    end );
    
    SetDefiningTripleOfUnderlyingQuiver( kC, DefiningTripleOfUnderlyingQuiver( C ) );
    
    if (CanCompute( C, "MorphismsOfExternalHom" ))
        
        ##
        AddBasisOfExternalHom( kC,
          
          function ( kC, source, target )
            local k, C;
            
            k = UnderlyingRing( kC );
            C = UnderlyingCategory( kC );
            
            return List( MorphismsOfExternalHom( C, ObjectDatum( kC, source ), ObjectDatum( kC, target ) ),
                            m -> MorphismConstructor( kC, source, PairGAP( [ One( k ) ], [ m ] ), target ) );
            
        end );
        
        ##
        AddCoefficientsOfMorphism( kC,
          
          function ( kC, alpha )
            local k, C, supp, coef, external_hom, indices;
            
            k = UnderlyingRing( kC );
            C = UnderlyingCategory( kC );
            
            supp = SupportMorphisms( alpha );
            coef = CoefficientsList( alpha );
            
            external_hom = MorphismsOfExternalHom( C, ObjectDatum( kC, Source( alpha ) ), ObjectDatum( kC, Target( alpha ) ) );
            
            indices = List( supp, mor -> PositionProperty( external_hom, m -> IsCongruentForMorphisms( C, m, mor ) ) );
            
            return List( (1):(Length( external_hom )),
                    function ( i )
                      local p;
                      
                      p = Position( indices, i );
                      
                      if (p != fail)
                          return coef[p];
                      else
                          return Zero( k );
                      end;
                      
                    end );
            
        end );
         
    end;
    
    INSTALL_VIEW_AND_DISPLAY_METHODS_IN_LINEAR_CLOSURES_OF_PATH_CATEGORIES_OR_THEIR_QUOTIENTS( kC );
    
    Append( kC.compiler_hints.category_attribute_names,
            [ "UnderlyingQuiver",
              "DefiningTripleOfUnderlyingQuiver",
              ] );
    
    Finalize( kC );
    
    return kC;
    
end ) );

##
@InstallMethod( LinearClosure,
          [ IsCategoryOfRows, IsPathCategory ],
          
  @FunctionWithNamedArguments(
  [
    [ "admissible_order", fail ],
    [ "colors", fail ]
  ],
  function ( CAP_NAMED_ARGUMENTS, rows, C )
    
    return LINEAR_CLOSURE_OF_PATH_CATEGORIES_OR_THEIR_QUOTIENTS( rows, C,
                admissible_order = admissible_order,
                colors = colors );
    
end ) );

##
@InstallMethod( LinearClosure,
          [ IsHomalgRing, IsPathCategory ],
          
  @FunctionWithNamedArguments(
  [
    [ "admissible_order", fail ],
    [ "colors", fail ]
  ],
  function ( CAP_NAMED_ARGUMENTS, k, C )
    
    return LinearClosure( CategoryOfRows( k ), C,
                admissible_order = admissible_order,
                colors = colors );
    
end ) );

##
@InstallMethod( getindex,
          [ IsHomalgRing, IsPathCategory ],
  
  function ( k, C )
    
    return LinearClosure( k, C );
    
end );

##
@InstallMethod( LinearClosure,
          [ IsCategoryOfRows, IsQuotientOfPathCategory ],
  
  function ( rows, C )
    
    return LINEAR_CLOSURE_OF_PATH_CATEGORIES_OR_THEIR_QUOTIENTS( rows, C );
    
end );

##
@InstallMethod( LinearClosure,
          [ IsHomalgRing, IsQuotientOfPathCategory ],
  
  function ( k, C )
    
    return LinearClosure( CategoryOfRows( k ), C );
    
end );

##
@InstallMethod( getindex,
          [ IsHomalgRing, IsQuotientOfPathCategory ],
  
  function ( k, C )
    
    return LinearClosure( k, C );
    
end );

##
@InstallMethod( SetOfObjects,
        "for a linear closure category",
        [ IsLinearClosure ],
        
  function( cat )
    
    return SetOfObjectsOfCategory( cat );
    
end );

##
@InstallMethod( SetOfGeneratingMorphisms,
        "for a linear closure category",
        [ IsLinearClosure ],
        
  function( cat )
    
    return SetOfGeneratingMorphismsOfCategory( cat );
    
end );

#= comment for Julia
##
@InstallMethod( AssignSetOfObjects,
        [ IsLinearClosure, IsString ],
        
  function( A, label )
    local names, objects, func;
    
    if (!( IsPathCategory( UnderlyingCategory( A ) ) || IsQuotientOfPathCategory( UnderlyingCategory( A ) ) ))
        TryNextMethod( );
    end;
    
    names = LabelsOfObjects( UnderlyingQuiver( A ) );
    
    objects = SetOfObjects( A );
    
    func = function( name, o )
              
              if (IntGAP( name ) != fail && label == "")
                  Error( "The second argument should be a non-empty string" );
              end;
              
              name = @Concatenation( label, ReplacedString( name, "-", "m" ) );
              
              MakeReadWriteGlobal( name );
              
              @BindGlobal( name, o );
              
              return 1;
              
            end;
            
    ListN( names, objects, func );
    
end );

##
@InstallMethod( AssignSetOfObjects,
        [ IsLinearClosure ],
        
  function( A )
    
    AssignSetOfObjects( A, "" );
    
end );

##
@InstallMethod( AssignSetOfGeneratingMorphisms,
        [ IsLinearClosure, IsString ],
        
  function( A, label )
    local names, morphisms, func;
    
    if (!( IsPathCategory( UnderlyingCategory( A ) ) || IsQuotientOfPathCategory( UnderlyingCategory( A ) ) ))
        TryNextMethod( );
    end;
    
    names = LabelsOfMorphisms( UnderlyingQuiver( A ) );
    
    morphisms = SetOfGeneratingMorphisms( A );
    
    func = function( name, m )
              
              if (IntGAP( name ) != fail && label == "")
                  Error( "The second argument should be a non-empty string" );
              end;
              
              name = @Concatenation( label, ReplacedString( name, "-", "m" ) );
              
              MakeReadWriteGlobal( name );
              
              @BindGlobal( name, m );
              
              return 1;
              
            end;
            
    ListN( names, morphisms, func );
    
end );

##
@InstallMethod( AssignSetOfGeneratingMorphisms,
        [ IsLinearClosure ],
        
  function( A )
    
    AssignSetOfGeneratingMorphisms( A, "" );
    
end );
# =#

##
@InstallGlobalFunction( "INSTALL_VIEW_AND_DISPLAY_METHODS_IN_LINEAR_CLOSURES_OF_PATH_CATEGORIES_OR_THEIR_QUOTIENTS",
  
  function ( kC )
    local C;
     
    C = UnderlyingCategory( kC );
    
    if (IsQuotientOfPathCategory( C ))
        
        C = AmbientCategory( C );
        
    end;
    
    ##
    InstallMethod( ViewString,
              [ ObjectFilter( kC ) ],
      
      function ( obj )
        
        return ViewString( ObjectDatum( obj ) );
        
    end );
    
    ##
    InstallMethod( DisplayString,
              [ ObjectFilter( kC ) ],
      
      obj -> @Concatenation( ViewString( obj ), "\n" )
    );
    
    ##
    InstallMethod( ViewString,
              [ MorphismFilter( kC ) ],
      
      function ( alpha )
        local kC, Q, bracket, coeffs, labels, datum_string;
        
        kC = CapCategory( alpha );
        
        Q = UnderlyingQuiver( C );
        
        bracket =
          function( str )
            if (Position( str, '+' ) == fail && Position( str, '-' ) == fail)
                return str;
            else
                return @Concatenation( "(", str, ")" );
            end;
        end;
        
        coeffs = List( CoefficientsList( alpha ), c -> bracket( @Concatenation( kC.colors.coeff, StringGAP( c ), kC.colors.reset ) ) );
        
        labels = List( SupportMorphisms( alpha ), m -> ( str -> str[ (1):(PositionSublist( str, @Concatenation( Q.colors.other, ":" ) ) - 1) ] )( ViewString( m ) ) );
        
        if (IsEmpty( labels ))
            
            datum_string = @Concatenation( kC.colors.coeff, "0", kC.colors.reset );
            
        else
            
            datum_string = JoinStringsWithSeparator( ListN( coeffs, labels, ( c, l ) -> @Concatenation( c, "*", l ) ), @Concatenation( kC.colors.reset, " + " ) );
            
        end;
        
        return @Concatenation(
              datum_string,
              Q.colors.other,
              ":",
              ViewString( UnderlyingOriginalObject( Source( alpha ) ) ),
              Q.colors.other,
              " → ",
              ViewString( UnderlyingOriginalObject( Target( alpha ) ) ) );
          
    end );
    
    ##
    InstallMethod( DisplayString,
              [ MorphismFilter( kC ) ],
      
      mor -> @Concatenation( ViewString( mor ), "\n" )
    );
    
end );

##
@InstallMethod( /,
          [ IsString, IsLinearClosure ],
  
  function ( name, kC )
    local cell;
    
    cell = UnderlyingCategory( kC )[name];
    
    if (IsCapCategoryObject( cell ))
        
        return ObjectConstructor( kC, cell );
        
    else
        
        return MorphismConstructor( kC,
                    ObjectConstructor( kC, Source( cell ) ),
                    PairGAP( [ One( UnderlyingRing( kC ) ) ], [ cell ] ),
                    ObjectConstructor( kC, Target( cell ) ) );
        
    end;
    
end );

##
#= comment for Julia
##
INSTALL_DOT_METHOD( IsLinearClosure );
# =#

##
@InstallMethod( DatumOfCellAsEvaluatableString,
        [ IsLinearClosureMorphism, IsList ],
        
  function( mor, list_of_evaluatable_strings )
    local datum;
    
    datum = MorphismDatum( mor );
    
    return @Concatenation(
                   "PairGAP( ", StringGAP( datum[1] ),
                   ", [ ",
                   JoinStringsWithSeparator( List( datum[2], e -> CellAsEvaluatableString( e, list_of_evaluatable_strings ) ), ", " ),
                   " ] )" );
    
end );

##
@InstallMethod( ExtendFunctorToAlgebroidData,
        "for a two categories and a pair of functions",
        [ IsLinearClosure, IsList, IsCapCategory ],
        
  function( LC, pair_of_funcs, category )
    local functor_on_objects, functor_on_morphisms,
          extended_functor_on_objects, extended_functor_on_morphisms;
    
    functor_on_objects = pair_of_funcs[1];
    functor_on_morphisms = pair_of_funcs[2];
    
    ## the code below is the doctrine-specific ur-algorithm for categories
    
    extended_functor_on_objects =
      function( objLC )
        local objC;
        
        objC = ObjectDatum( LC, objLC );
        
        return functor_on_objects( objC );
        
    end;
    
    extended_functor_on_morphisms =
      function( source, morLC, target )
        local morC;
        
        morC = MorphismDatum( LC, morLC );
        
        return LinearCombinationOfMorphisms( category,
                       source,
                       morC[1], ## coeffs
                       List( morC[2], mor ->
                             functor_on_morphisms(
                                     functor_on_objects( Source( mor ) ),
                                     mor,
                                     functor_on_objects( Target( mor ) ) ) ),
                       target );
        
    end;
    
    return Triple( LC,
                   PairGAP( extended_functor_on_objects, extended_functor_on_morphisms ),
                   category );
    
end );

##
@InstallMethod( EvaluateLinearClosureEndomorphism,
        "for a linear closure category, a morphism therein, a linear category, an object therein, and a list of endomorphisms thereof",
        [ IsLinearClosure, IsLinearClosureMorphism, IsCapCategory, IsCapCategoryObject, IsList ],
        
  function( linear_closure, morphism, V, rep_obj, rep_mors )
    local functor_on_obj, functor_on_mors, functor;
    
    functor_on_obj =
      function( objC )
        
        return rep_obj;
        
    end;
    
    functor_on_mors =
      function( source, morC, target )
        
        return PreComposeList( source, rep_mors[MorphismIndices( morC )], target );
        
    end;
    
    functor = ExtendFunctorToAlgebroidData( linear_closure,
                       PairGAP( functor_on_obj, functor_on_mors ),
                       V );
    
    return functor[2][2]( rep_obj, morphism, rep_obj );
    
end );
