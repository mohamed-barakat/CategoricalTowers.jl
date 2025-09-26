# SPDX-License-Identifier: GPL-2.0-or-later
# QuotientCategories: Quotient categories
#
# Implementations
#

##
@InstallValueConst( CAP_INTERNAL_METHOD_NAME_LIST_FOR_QUOTIENT_CATEGORY,
  [# IsCapCategory
   "IdentityMorphism",
   "IsEndomorphism",
   "IsEqualForMorphisms",
   "IsEqualForObjects",
   "IsEqualForCacheForMorphisms",
   "IsEqualForCacheForObjects",
   "IsEqualToIdentityMorphism",
   "IsEqualToZeroMorphism",
   "PostCompose",
   "PreCompose",
   "IsWellDefinedForObjects",
   "IsWellDefinedForMorphisms",
   
   # IsAbCategory
   "AdditionForMorphisms",
   "AdditiveInverseForMorphisms",
   "SubtractionForMorphisms",
   "ZeroMorphism",
   #"MultiplyWithElementOfCommutativeRingForMorphisms",
   
   # IsCartesianCategory
   "DirectProduct",
   "ProjectionInFactorOfDirectProductWithGivenDirectProduct",
   "UniversalMorphismIntoDirectProductWithGivenDirectProduct",
   "TerminalObject",
   "UniversalMorphismIntoTerminalObjectWithGivenTerminalObject",
   
   # IsCocartesianCategory
   "Coproduct",
   "InjectionOfCofactorOfCoproductWithGivenCoproduct",
   "UniversalMorphismFromCoproductWithGivenCoproduct",
   "InitialObject",
   "UniversalMorphismFromInitialObjectWithGivenInitialObject",
   
   # IsAdditiveCategory
   "ComponentOfMorphismFromDirectSum",
   "ComponentOfMorphismIntoDirectSum",
   "DirectSum",
   "DirectSumCodiagonalDifference",
   "DirectSumDiagonalDifference",
   "DirectSumFunctorialWithGivenDirectSums",
   "DirectSumProjectionInPushout",
   "InjectionOfCofactorOfDirectSum",
   "InjectionOfCofactorOfDirectSumWithGivenDirectSum",
   "IsomorphismFromCoproductToDirectSum",
   "IsomorphismFromDirectProductToDirectSum",
   "IsomorphismFromDirectSumToCoproduct",
   "IsomorphismFromDirectSumToDirectProduct",
   "MorphismBetweenDirectSumsWithGivenDirectSums",
   "ProjectionInFactorOfDirectSum",
   "ProjectionInFactorOfDirectSumWithGivenDirectSum",
   "UniversalMorphismFromDirectSum",
   "UniversalMorphismFromDirectSumWithGivenDirectSum",
   "UniversalMorphismIntoDirectSum",
   "UniversalMorphismIntoDirectSumWithGivenDirectSum",
   "UniversalMorphismFromZeroObject",
   "UniversalMorphismFromZeroObjectWithGivenZeroObject",
   "UniversalMorphismIntoZeroObject",
   "UniversalMorphismIntoZeroObjectWithGivenZeroObject",
   "ZeroObject",
   "ZeroObjectFunctorial",
   ] );

##
@InstallMethod( QuotientCategory,
        [ IsRecord ],
        
  @FunctionWithNamedArguments(
  [
    [ "FinalizeCategory", true ],
  ],
  function( CAP_NAMED_ARGUMENTS, record )
    local congruence_func, ambient_cat, name, category_filter, category_object_filter, category_morphism_filter,
          object_constructor, object_datum, morphism_constructor, morphism_datum,
          create_func_bool, create_func_object, create_func_morphism,
          list_of_operations_to_install, commutative_ring,
          properties, supports_empty_limits, quotient_cat;
    
    if (@not @IsBound( record.congruence_func ))
        Error( "the record passed to the category constructor 'QuotientCategory' is missing a component 'congruence_func'!\n" );
    end;
    
    if (@not @IsBound( record.nr_arguments_of_congruence_func ))
        Error( "the record passed to the category constructor 'QuotientCategory' is missing a component 'nr_arguments_of_congruence_func'!\n" );
    end;
    
    record = ShallowCopy( record );
    
    ambient_cat = record.underlying_category;
    
    if (record.nr_arguments_of_congruence_func == 1)
      
      record.nr_arguments_of_congruence_func = 2;
      
      congruence_func = record.congruence_func;
      
      record.congruence_func = ( alpha, beta ) -> congruence_func( SubtractionForMorphisms( ambient_cat, alpha, beta ) );
      
      return QuotientCategory( record; FinalizeCategory = FinalizeCategory );
      
    end;
     
    if (@IsBound( record.name ))
      name = record.name;
    else
      name = @Concatenation( "QuotientCategory( ", Name( ambient_cat ), " )" );
    end;
    
    if (@IsBound( record.category_filter ))
      category_filter = record.category_filter;
    else
      category_filter = IsQuotientCategory;
    end;
    
    if (@IsBound( record.category_object_filter ))
      category_object_filter = record.category_object_filter;
    else
      category_object_filter = IsQuotientCategoryObject;
    end;
   
    if (@IsBound( record.category_morphism_filter ))
      category_morphism_filter = record.category_morphism_filter;
    else
      category_morphism_filter = IsQuotientCategoryMorphism;
    end;
    
    object_constructor = ( quotient_cat, datum ) -> CreateCapCategoryObjectWithAttributes( quotient_cat, UnderlyingCell, datum );
    
    object_datum = ( quotient_cat, o ) -> UnderlyingCell( o );
    
    morphism_constructor = ( quotient_cat, S, datum, R ) -> CreateCapCategoryMorphismWithAttributes( quotient_cat, S, R, UnderlyingCell, datum );
    
    morphism_datum = ( quotient_cat, m ) -> UnderlyingCell( m );
    
    list_of_operations_to_install = Intersection( ListPrimitivelyInstalledOperationsOfCategory( ambient_cat ), CAP_INTERNAL_METHOD_NAME_LIST_FOR_QUOTIENT_CATEGORY );
    
    if (HasCommutativeRingOfLinearCategory( ambient_cat ))
        commutative_ring = CommutativeRingOfLinearCategory( ambient_cat );
    else
        commutative_ring = fail;
    end;
    
    properties = [ "IsObjectFiniteCategory",
                    "IsFinitelyPresentedCategory",
                    "IsFiniteCategory",
                    "IsCocartesianCategory",
                    "IsCartesianCategory",
                    "IsAbCategory",
                    "IsLinearCategoryOverCommutativeRing",
                    "IsAdditiveCategory",
                    ];
    
    if (HasIsLinearCategoryOverCommutativeRingWithFinitelyGeneratedFreeExternalHoms( ambient_cat ) &&
         IsLinearCategoryOverCommutativeRingWithFinitelyGeneratedFreeExternalHoms( ambient_cat ) &&
         HasCommutativeRingOfLinearCategory( ambient_cat ))
        
        if (HasIsFieldForHomalg( commutative_ring ) && IsFieldForHomalg( commutative_ring ))
            Add( properties, "IsLinearCategoryOverCommutativeRingWithFinitelyGeneratedFreeExternalHoms" );
        end;
        
    end;
    
    properties = Intersection( ListKnownCategoricalProperties( ambient_cat ), properties );
    
    if (@IsBound( record.extra_properties ))
        properties = SortedList( @Concatenation( properties, record.extra_properties ) );
    end;
    
    create_func_bool =
          function ( name, quotient_cat )
            return PairGAP( """
                    function( input_arguments... )
                      
                      return CallFuncList( operation_name, List( @NTupleGAP( number_of_arguments, input_arguments... )[ (2):(number_of_arguments) ], UnderlyingCell ) );
                      
                    end
                    """, OperationWeight( ambient_cat, name ) );
          end;
    
    create_func_object =
          function ( name, quotient_cat )
            
            if (name in [ "TerminalObject", "InitialObject", "ZeroObject" ])
              return PairGAP( """
                      function( input_arguments... )
                        
                        return ObjectConstructor( cat, operation_name( AmbientCategory( cat ) ) );
                        
                      end
                      """, OperationWeight( ambient_cat, name ) );
            
            elseif (name in [ "DirectProduct", "Coproduct", "DirectSum" ])
              
              ## a constructor for universal objects: DirectSum
              return PairGAP( """
                      function ( input_arguments... )
                        local i_arg;
                        
                        i_arg = @NTupleGAP( number_of_arguments, input_arguments... );
                        
                        return ObjectConstructor( cat, operation_name( AmbientCategory( cat ), List( i_arg[2], UnderlyingCell ) ) );
                        
                      end
                      """, OperationWeight( ambient_cat, name ) );
            
            else
              
              Print( "WARNING: the category constructor 'QuotientCategory' cannot deal with ", name, " yet\n" );
              return "ReturnFail";
            
            end;
          
          end;
    
    create_func_morphism =
      function ( name, cat )
        local info;
        
        info = CAP_INTERNAL_METHOD_NAME_RECORD[name];
        
        return PairGAP( ReplacedStringViaRecord(
                """
                function ( input_arguments... )
                  local underlying_cat, i_arg;
                  
                  underlying_cat = AmbientCategory( cat );
                  
                  i_arg = @NTupleGAP( number_of_arguments, input_arguments... );
                  
                  return MorphismConstructor( cat, top_source, operation_name( underlying_cat, sequence_of_arguments... ), top_range );
        
        end
        """,
        @rec( sequence_of_arguments =
             List( (2):(Length( info.filter_list )),
                   function( j )
                     local type;
                     
                     type = info.filter_list[j];
                     
                     if (type == "integer")
                         return @Concatenation( "i_arg[", StringGAP( j ), "]" );
                     elseif (type in [ "object", "morphism" ])
                         return @Concatenation( "UnderlyingCell( i_arg[", StringGAP( j ), "] )" );
                     elseif (type in [ "list_of_objects", "list_of_morphisms" ])
                         return @Concatenation( "List( i_arg[", StringGAP( j ), "], UnderlyingCell )" );
                     elseif (type == "list_of_lists_of_morphisms")
                         return @Concatenation( "List( i_arg[", StringGAP( j ), "], x -> List( x, y -> UnderlyingCell( y ) ) )" );
                     else
                         Error( "can only deal with \"integer\", \"object\", \"morphism\", \"list_of_objects\", \"list_of_morphisms\"" );
                     end;
                     
                  end ) ) ), OperationWeight( ambient_cat, name ) );
    
    end;
    
    if (@IsBound( ambient_cat.supports_empty_limits ))
        supports_empty_limits = ambient_cat.supports_empty_limits;
    else
        supports_empty_limits = false;
    end;
    
    quotient_cat =
      CategoryConstructor(
        @rec( name = name,
                   category_filter = category_filter,
                   category_object_filter = category_object_filter,
                   category_morphism_filter = category_morphism_filter,
                   #commutative_ring_of_linear_category = commutative_ring,
                   properties = properties,
                   object_constructor = object_constructor,
                   object_datum = object_datum,
                   morphism_constructor = morphism_constructor,
                   morphism_datum = morphism_datum,
                   underlying_category_getter_string = "AmbientCategory",
                   underlying_category = ambient_cat,
                   list_of_operations_to_install = list_of_operations_to_install,
                   supports_empty_limits = supports_empty_limits,
                   create_func_bool = create_func_bool,
                   create_func_object = create_func_object,
                   create_func_morphism = create_func_morphism ) );
    
    SetAmbientCategory( quotient_cat, ambient_cat );
    
    if (CanCompute( ambient_cat, "SetOfObjectsOfCategory" ))
        
        ##
        AddSetOfObjectsOfCategory( quotient_cat,
          function( quotient_cat )
            local ambient_cat, objects;
            
            ambient_cat = AmbientCategory( quotient_cat );
            
            objects = SetOfObjects( ambient_cat );
            
            return List( objects, o -> ObjectConstructor( quotient_cat, o ) );
            
        end, OperationWeight( ambient_cat, "SetOfObjectsOfCategory" ) );
        
    end;
    
    SetQuotientCategoryCongruenceFunction( quotient_cat, record.congruence_func );
    
    if (commutative_ring != fail)
        SetCommutativeRingOfLinearCategory( quotient_cat, commutative_ring );
    end;
    
    AddIsCongruentForMorphisms( quotient_cat,
      function( quotient_cat, phi, psi )
      
        return QuotientCategoryCongruenceFunction( quotient_cat )( UnderlyingCell( phi ), UnderlyingCell( psi ) );
        
    end );
    
    if (CanCompute( ambient_cat, "MultiplyWithElementOfCommutativeRingForMorphisms" ))
        
        ##
        AddMultiplyWithElementOfCommutativeRingForMorphisms( quotient_cat,
          function( quotient_cat, r, phi )
            
            return MorphismConstructor( quotient_cat, Source( phi ), MultiplyWithElementOfCommutativeRingForMorphisms( AmbientCategory( quotient_cat ), r, UnderlyingCell( phi ) ), Target( phi ) );
            
        end, OperationWeight( ambient_cat, "MultiplyWithElementOfCommutativeRingForMorphisms" ) );
    
    end;
    
    ADD_FUNCTIONS_OF_RANDOM_METHODS_TO_QUOTIENT_CATEGORY( quotient_cat );
    
    if (FinalizeCategory)
      
      Finalize( quotient_cat );
      
    end;
    
    return quotient_cat;
    
end ) );


@InstallGlobalFunction( ADD_FUNCTIONS_OF_RANDOM_METHODS_TO_QUOTIENT_CATEGORY,
  function( quotient_cat )
    local ambient_cat;
    
    ambient_cat = AmbientCategory( quotient_cat );
    
    if (CanCompute( ambient_cat, "RandomObjectByInteger" ))
        
        AddRandomObjectByInteger( quotient_cat,
            function( quotient_cat, i )
              return ObjectConstructor( quotient_cat, RandomObjectByInteger( AmbientCategory( quotient_cat ), i ) );
        end, OperationWeight( ambient_cat, "RandomObjectByInteger" ) );
        
    end;
    
    if (CanCompute( ambient_cat, "RandomMorphismWithFixedSourceAndRangeByInteger" ))
        
        AddRandomMorphismWithFixedSourceAndRangeByInteger( quotient_cat,
            function( quotient_cat, S, R, i )
                local alpha;
                alpha = RandomMorphismWithFixedSourceAndRangeByInteger( AmbientCategory( quotient_cat ), UnderlyingCell( S ), UnderlyingCell( R ), i );
                return MorphismConstructor( quotient_cat,  ObjectConstructor( quotient_cat, Source( alpha ) ), alpha, ObjectConstructor( quotient_cat, Target( alpha ) ) );
        end, OperationWeight( ambient_cat, "RandomMorphismWithFixedSourceAndRangeByInteger" ) );
        
    end;
    
    if (CanCompute( ambient_cat, "RandomMorphismWithFixedSourceByInteger" ))
        
        AddRandomMorphismWithFixedSourceByInteger( quotient_cat,
            function( quotient_cat, S, i )
                local alpha;
                alpha = RandomMorphismWithFixedSourceByInteger( AmbientCategory( quotient_cat ), UnderlyingCell( S ), i );
                return MorphismConstructor( quotient_cat,  S, alpha, ObjectConstructor( quotient_cat, Target( alpha ) ) );
        end, OperationWeight( ambient_cat, "RandomMorphismWithFixedSourceByInteger" ) );
        
    end;
    
    if (CanCompute( ambient_cat, "RandomMorphismWithFixedRangeByInteger" ))
        
        AddRandomMorphismWithFixedRangeByInteger( quotient_cat,
            function( quotient_cat, R, i )
                local alpha;
                alpha = RandomMorphismWithFixedRangeByInteger( AmbientCategory( quotient_cat ), UnderlyingCell( R ), i );
                return MorphismConstructor( quotient_cat,  ObjectConstructor( quotient_cat, Source( alpha ) ), alpha, R );
        end, OperationWeight( ambient_cat, "RandomMorphismWithFixedRangeByInteger" ) );
        
    end;
    
    if (CanCompute( ambient_cat, "RandomMorphismByInteger" ))
        
        AddRandomMorphismByInteger( quotient_cat,
            function( quotient_cat, i )
                local alpha;
                alpha = RandomMorphismByInteger( AmbientCategory( quotient_cat ), i );
                return MorphismConstructor( quotient_cat,  ObjectConstructor( quotient_cat, Source( alpha ) ), alpha, ObjectConstructor( quotient_cat, Target( alpha ) ) );
        end, OperationWeight( ambient_cat, "RandomMorphismByInteger" ) );
    end;
    
    if (CanCompute( ambient_cat, "RandomObjectByList" ))
        
        AddRandomObjectByList( quotient_cat,
            function( quotient_cat, L )
              return ObjectConstructor( quotient_cat, RandomObjectByList( AmbientCategory( quotient_cat ), L ) );
        end, OperationWeight( ambient_cat, "RandomObjectByList" ) );
        
    end;
    
    if (CanCompute( ambient_cat, "RandomMorphismWithFixedSourceAndRangeByList" ))
        
        AddRandomMorphismWithFixedSourceAndRangeByList( quotient_cat,
            function( quotient_cat, S, R, L )
                local alpha;
                alpha = RandomMorphismWithFixedSourceAndRangeByList( AmbientCategory( quotient_cat ), UnderlyingCell( S ), UnderlyingCell( R ), L );
                return MorphismConstructor( quotient_cat,  ObjectConstructor( quotient_cat, Source( alpha ) ), alpha, ObjectConstructor( quotient_cat, Target( alpha ) ) );
        end, OperationWeight( ambient_cat, "RandomMorphismWithFixedSourceAndRangeByList" ) );
        
    end;
    
    if (CanCompute( ambient_cat, "RandomMorphismWithFixedSourceByList" ))
        
        AddRandomMorphismWithFixedSourceByList( quotient_cat,
            function( quotient_cat, S, L )
                local alpha;
                alpha = RandomMorphismWithFixedSourceByList( AmbientCategory( quotient_cat ), UnderlyingCell( S ), L );
                return MorphismConstructor( quotient_cat,  S, alpha, ObjectConstructor( quotient_cat, Target( alpha ) ) );
        end, OperationWeight( ambient_cat, "RandomMorphismWithFixedSourceByList" ) );
        
    end;
    
    if (CanCompute( ambient_cat, "RandomMorphismWithFixedRangeByList" ))
        
        AddRandomMorphismWithFixedRangeByList( quotient_cat,
            function( quotient_cat, R, L )
                local alpha;
                alpha = RandomMorphismWithFixedRangeByList( AmbientCategory( quotient_cat ), UnderlyingCell( R ), L );
                return MorphismConstructor( quotient_cat,  ObjectConstructor( quotient_cat, Source( alpha ) ), alpha, R );
        end, OperationWeight( ambient_cat, "RandomMorphismWithFixedRangeByList" ) );
        
    end;
    
    if (CanCompute( ambient_cat, "RandomMorphismByList" ))
        
        AddRandomMorphismByList( quotient_cat,
            function( quotient_cat, L )
                local alpha;
                alpha = RandomMorphismByList( AmbientCategory( quotient_cat ), L );
                return MorphismConstructor( quotient_cat,  ObjectConstructor( quotient_cat, Source( alpha ) ), alpha, ObjectConstructor( quotient_cat, Target( alpha ) ) );
        end, OperationWeight( ambient_cat, "RandomMorphismByList" ) );
        
    end;
    
end );

##
@InstallMethod( SetOfObjects,
        "for a quotient category",
        [ IsQuotientCategory ],
        
  function( cat )
    
    return SetOfObjectsOfCategory( cat );
    
end );

#= comment for Julia
##
@InstallMethod( \.,
        [ IsQuotientCategory, IsPosInt ],
  
  ( quotient_cat, string_as_int ) -> AmbientCategory( quotient_cat )[NameRNam( string_as_int] ) / quotient_cat
);
# =#

##
@InstallMethod( /,
            [ IsCapCategoryMorphism, IsQuotientCategory ],
  
  ( alpha, quotient_cat ) -> MorphismConstructor( quotient_cat, Source( alpha ) / quotient_cat, alpha, Target( alpha ) / quotient_cat )
);

##
@InstallMethod( /,
            [ IsString, IsQuotientCategory ],
  
  ( str, quotient_cat ) -> (str / AmbientCategory( quotient_cat )) / quotient_cat
);

##
@InstallMethod( ExtendFunctorToQuotientCategoryData,
        "for a quotient category, a pair of functions, and a category",
        [ IsQuotientCategory, IsList, IsCapCategory ],
        
  function( QC, pair_of_funcs, category )
    local functor_on_objects, functor_on_morphisms,
          extended_functor_on_objects, extended_functor_on_morphisms;
    
    functor_on_objects = pair_of_funcs[1];
    functor_on_morphisms = pair_of_funcs[2];
    
    ## the code below is the doctrine-specific ur-algorithm for categories
    
    extended_functor_on_objects =
      function( objQC )
        local objC;
        
        objC = ObjectDatum( QC, objQC );
        
        return functor_on_objects( objC );
        
    end;
    
    extended_functor_on_morphisms =
      function( source, morQC, target )
        local morC;
        
        morC = MorphismDatum( QC, morQC );
        
        return functor_on_morphisms( source, morC, target );
        
    end;
    
    return Triple( QC,
                   PairGAP( extended_functor_on_objects, extended_functor_on_morphisms ),
                   category );
    
end );

##################################
##
## View & Display
##
##################################

##
@InstallMethod( DisplayString,
        [ IsQuotientCategoryObject ],
        
  function ( a )
    
    return @Concatenation( DisplayString( ObjectDatum( a ) ), "\nAn object in ", Name( CapCategory( a ) ), " given by the above data\n" );
    
end );

##
@InstallMethod( DisplayString,
        [ IsQuotientCategoryMorphism ],
        
  function ( phi )
    
    return @Concatenation( DisplayString( MorphismDatum( phi ) ), "\nA morphism in ", Name( CapCategory( phi ) ), " given by the above data\n" );
    
end );

##
@InstallMethod( LaTeXOutput,
            [ IsQuotientCategoryObject ],
  
  o -> LaTeXOutput( UnderlyingCell( o ) )
);

##
@InstallMethod( LaTeXOutput,
            [ IsQuotientCategoryMorphism ],
  
  alpha -> LaTeXOutput( UnderlyingCell( alpha ) )
);
