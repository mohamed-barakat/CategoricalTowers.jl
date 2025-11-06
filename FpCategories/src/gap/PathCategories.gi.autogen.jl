# SPDX-License-Identifier: GPL-2.0-or-later
# FpCategories: Finitely presented categories by generating quivers and relations
#
# Implementations
#

##
@InstallMethod( PathCategory,
          [ IsFinQuiver ],
  
  @FunctionWithNamedArguments(
  [
    [ "admissible_order", fail ],
    [ "FinalizeCategory", false ],
    [ "range_of_HomStructure", fail ],
  ],
  function( CAP_NAMED_ARGUMENTS, q )
    local name, C, range_cat;
    
    name = @Concatenation( "PathCategory( ", Name( q ), " )" );
    
    C = CreateCapCategoryWithDataTypes( name,
                 IsPathCategory,
                 IsPathCategoryObject,
                 IsPathCategoryMorphism,
                 IsCapCategoryTwoCell,
                 IsBigInt,
                 CapJitDataTypeOfNTupleOf( 2,
                         IsBigInt,
                         CapJitDataTypeOfListOf( IsBigInt ) ),
                 fail
                ; overhead = false );
    
    C.category_as_first_argument = true;
    
    if (admissible_order == fail)
      
      C.admissible_order = "dp";
      
    else
      
      C.admissible_order = admissible_order;
      
    end;
    
    SetIsFinitelyPresentedCategory( C, true );
    
    SetUnderlyingQuiver( C, q );
    
    SetDefiningTripleOfUnderlyingQuiver( C,
            Triple( NumberOfObjects( q ),
                    NumberOfMorphisms( q ),
                    ListN( IndicesOfSources( q ), IndicesOfTargets( q ), ( s, t ) -> PairGAP( -1 + s, -1 + t ) ) ) );
    
    C.compiler_hints =
      @rec( category_attribute_names =
           [ "UnderlyingQuiver",
             "DefiningTripleOfUnderlyingQuiver",
             ],
           );
    
    ##
    AddObjectConstructor( C,
      function ( C, obj_index )
        
        return SetOfObjects( C )[obj_index];
        
    end );
    
    ##
    AddObjectDatum( C,
      function ( C, obj )
        
        return ObjectIndex( obj );
        
    end );
    
    ##
    AddIsWellDefinedForObjects( C,
      function ( C, obj )
        
        return true;
        
    end );
    
    ##
    AddIsEqualForObjects( C,
      function ( C, obj_1, obj_2 )
        
        return IsIdenticalObj( obj_1, obj_2 );
        
    end );
    
    ##
    AddMorphismConstructor( C,
      function ( C, source, datum, target )
        
        #% CAP_JIT_DROP_NEXT_STATEMENT
        @Assert( 0, ForAll( datum[2], IsBigInt ) );
        
        return CreateCapCategoryMorphismWithAttributes( C,
                       source, target,
                       MorphismLength, datum[1],
                       MorphismIndices, datum[2] );
        
    end );
     
    ##
    AddMorphismDatum( C,
      function ( C, mor )
        
        return PairGAP( MorphismLength( mor ), MorphismIndices( mor ) );
        
    end );
    
    ##
    AddIsWellDefinedForMorphisms( C,
      function ( C, mor )
        local q, l, s;
        
        q = UnderlyingQuiver( C );
        
        l = MorphismLength( mor );
        s = MorphismSupport( mor );
        
        return l == Length( s ) &&
               ( ( l == 0 && IsEndomorphism( C, mor ) ) ||
                 ( ObjectIndex( Source( mor ) ) == ObjectIndex( Source( First( s ) ) ) &&
                   ObjectIndex( Target( mor ) ) == ObjectIndex( Target( Last( s ) ) ) ) ) &&
               ForAll( (1):(l - 1), j -> Target( s[j] ) == Source( s[j+1] ) );
        
    end );
    
    ##
    AddIsEqualForMorphisms( C,
      function ( C, mor_1, mor_2 )
        
        return IsEqualForObjects( C, Source( mor_1 ), Source( mor_2 ) ) &&
                IsEqualForObjects( C, Target( mor_1 ), Target( mor_2 ) ) &&
                MorphismLength( mor_1 ) == MorphismLength( mor_2 ) &&
                MorphismIndices( mor_1 ) == MorphismIndices( mor_2 );
        
    end );
    
    ##
    AddIsCongruentForMorphisms( C,
      function ( C, mor_1, mor_2 )
        
        return IsEqualForMorphisms( C, mor_1, mor_2 );
        
    end );
    
    AddIdentityMorphism( C,
      function ( C, obj )
        
        return MorphismConstructor( C,
                       obj,
                       PairGAP( BigInt( 0 ),
                             CapJitTypedExpression( [ ], ( ) -> CapJitDataTypeOfListOf( IsBigInt ) ) ),
                       obj );
        
    end );
    
    AddPreCompose( C,
      function ( C, mor_1, mor_2 )
        
        return MorphismConstructor( C,
                      Source( mor_1 ),
                      PairGAP( MorphismLength( mor_1 ) + MorphismLength( mor_2 ),
                            @Concatenation( MorphismIndices( mor_1 ), MorphismIndices( mor_2 ) ) ),
                      Target( mor_2 ) );
        
    end );
    
    ##
    AddSetOfObjectsOfCategory( C,
      function ( C )
        
        return List( (1):(NumberOfObjects( UnderlyingQuiver( C ) )), obj_index ->
                     CreateCapCategoryObjectWithAttributes( C, ObjectIndex, obj_index ) );
        
    end );
    
    ##
    AddSetOfGeneratingMorphismsOfCategory( C,
      function ( C )
        local q, s, t;
        
        q = UnderlyingQuiver( C );
        
        s = IndicesOfSources( q );
        t = IndicesOfTargets( q );
        
        return List( (1):(NumberOfMorphisms( q )), mor ->
                     MorphismConstructor( C,
                             SetOfObjects( C )[s[mor]],
                             PairGAP( BigInt( 1 ), [ BigInt( mor ) ] ),
                             SetOfObjects( C )[t[mor]] ) );
        
    end );
    
    ##
    AddRandomObjectByInteger( C,
      function ( C, n )
        
        return Random( SetOfObjects( C ) );
        
    end );
    
    ##
    AddRandomMorphismWithFixedSourceByInteger( C,
      function ( C, obj, n )
        local s, t, m;
        
        s = ObjectIndex( obj );
        t = Random( (1):(NumberOfObjects( UnderlyingQuiver( C ) )) );
        
        m = ExternalHomsWithGivenLength( C, BigInt( 0 ), BigInt( n ) )[s][t];
        
        if (m == [])
              return IdentityMorphism( C, obj );
        else
              return Random( m );
        end;
        
    end );
    
    ##
    AddRandomMorphismWithFixedRangeByInteger( C,
      function ( C, obj, n )
        local s, t, m;
        
        s = Random( (1):(NumberOfObjects( UnderlyingQuiver( C ) )) );
        t = ObjectIndex( obj );
        
        m = ExternalHomsWithGivenLength( C, BigInt( 0 ), BigInt( n ) )[s][t];
        
        if (m == [])
              return IdentityMorphism( C, obj );
        else
              return Random( m );
        end;
        
    end );
    
    ##
    AddRandomMorphismWithFixedSourceAndRangeByInteger( C,
      function ( C, obj_1, obj_2, n )
        local s, t, p;
        
        s = ObjectIndex( obj_1 );
        t = ObjectIndex( obj_2 );
        
        p = PositionProperty( (1):(n + 1), i -> ExternalHomsWithGivenLength( C, BigInt( n + 1 - i ) )[s][t] != [] );
        
        if (p == fail)
              Error( "The Hom-set between the specified source and target objects is empty!\n" );
        else
              return Random( ExternalHomsWithGivenLength( C, BigInt( n + 1 - p ) )[s][t] );
        end;
        
    end );
    
    # Homomorphism Structure - Only for path categories with underlying acyclic quivers
    
    if (IsFinitePathCategory( C ) )
        
        SetIsFiniteCategory( C, true );
        
        range_cat = range_of_HomStructure;
        
        if (@not IsSkeletalCategoryOfFiniteSets( range_cat ))
            range_cat = SkeletalFinSets;
        end;
        
        SET_RANGE_CATEGORY_Of_HOMOMORPHISM_STRUCTURE( C, range_cat );
        
        @Assert( 0, IsIdenticalObj( RangeCategoryOfHomomorphismStructure( C ), range_cat ) );
        
        AddMorphismsOfExternalHom( C,
          function ( C, obj_1, obj_2 )
            local s, t;
            
            s = ObjectIndex( obj_1 );
            t = ObjectIndex( obj_2 );
            
            return ExternalHoms( C )[s][t];
            
        end );
        
    else
        
        SetIsFiniteCategory( C, false );
        
    end;
    
    Finalize( C );
    
    return C;
    
end ) );

##
@InstallMethod( SetOfObjects,
        "for a path category",
        [ IsPathCategory ],
        
  function( cat )
    
    return SetOfObjectsOfCategory( cat );
    
end );

##
@InstallMethod( SetOfGeneratingMorphisms,
        "for a path category",
        [ IsPathCategory ],
        
  function( cat )
    
    return SetOfGeneratingMorphismsOfCategory( cat );
    
end );

##
@InstallMethod( DecompositionIndicesOfMorphism,
        "for a path category and a morphism therein",
        [ IsPathCategory, IsPathCategoryMorphism ],
        
  function( C, mor )
    
    return -1 + MorphismDatum( C, mor )[2];
    
end );

##
@InstallMethod( DecompositionOfMorphismInCategory,
        "for a morphism in a path category",
        [ IsPathCategoryMorphism ],
        
  function( mor )
    local C, dec;
    
    C = CapCategory( mor );
    
    dec = SetOfGeneratingMorphisms( C )[1 + DecompositionIndicesOfMorphism( C, mor )];
    
    if (ForAny( dec, IsEqualToIdentityMorphism ))
        Error( "one of the generating morphisms is an identity morphism\n" );
    end;
    
    return dec;
    
end );

##
@InstallMethod( ExternalHomsWithGivenLengthDataOp,
        [ IsPathCategory, IsBigInt ],
  
  function ( C, len )
    local q, nr_objs, nr_gmors, gmors, data, prev_data, r, j, s;
    
    q = UnderlyingQuiver( C );
    
    nr_objs = NumberOfObjects( q );
    nr_gmors = NumberOfMorphisms( q );
    
    gmors = List( (1):(nr_gmors), j -> PairGAP( IndicesOfSources( q )[j], IndicesOfTargets( q )[j] ) );
    
    data = List( (1):(nr_objs), s -> List( (1):(nr_objs), t -> [ ] ) );
    
    if (len == 0)
      
      for r in (1):(nr_objs)
          data[r][r] = @Concatenation( data[r][r], [ [ ] ] );
      end;
      
    else
      
      prev_data = ExternalHomsWithGivenLengthData( C, len - 1 );
      
      # It is better to get the morphisms already sorted from max to min, hence:
      
      if (C.admissible_order == "Dp")
        
        for r in (1):(nr_objs)
          for j in (1):(nr_gmors)
            data[gmors[j][1]][r] = @Concatenation( data[gmors[j][1]][r], List( prev_data[gmors[j][2]][r], l -> @Concatenation( [ j ], l ) ) );
          end;
        end;
        
      elseif (C.admissible_order == "dp")
        
        for s in (1):(nr_objs)
          for j in (1):(nr_gmors)
            data[s][gmors[j][2]] = @Concatenation( data[s][gmors[j][2]], List( prev_data[s][gmors[j][1]], l -> @Concatenation( l, [ j ] ) ) );
          end;
        end;
        
      end;
      
    end;
    
    return data;
    
end );

##
@InstallMethod( ExternalHomsWithGivenLengthOp,
        [ IsPathCategory, IsBigInt ],
  
  function ( C, len )
    local q, supports;
    
    q = UnderlyingQuiver( C );
    
    supports = ExternalHomsWithGivenLengthData( C, len );
    
    return List( (1):(NumberOfObjects( q )), s ->
                 List( (1):(NumberOfObjects( q )), t ->
                       List( supports[s][t], supp ->
                             MorphismConstructor( C,
                                     SetOfObjects( C )[s],
                                     PairGAP( len, List( supp, i -> BigInt( i ) ) ),
                                     SetOfObjects( C )[t] ) ) ) );
    
end );

##
@InstallMethod( ExternalHomsWithGivenLength,
            "for path categories",
        [ IsCapCategory, IsBigInt, IsBigInt ],
  
  function ( C, l, u )
    local nr_objs;
    
    nr_objs = NumberOfObjects( UnderlyingQuiver( C ) );
    
    return LazyHList( (1):(nr_objs),
              s -> LazyHList( (1):(nr_objs),
                      t -> @Concatenation( List( (l):(u),
                              len -> ExternalHomsWithGivenLength( C, u + l - len )[s][t] ) ) ) );
    
end );

##
@InstallMethod( UnderlyingQuiverObject,
          [ IsPathCategoryObject ],
  
  function ( obj )
    local q;
    
    q = UnderlyingQuiver( CapCategory( obj ) );
    
    return SetOfObjects( q )[ObjectIndex( obj )];
    
end );

##
@InstallMethod( LaTeXOutput,
          [ IsPathCategoryObject ],
  
  function ( obj )
    
    return LaTeXOutput( UnderlyingQuiverObject( obj ) );
    
end );

##
@InstallMethod( ObjectLabel,
          [ IsPathCategoryObject ],
  
  function ( obj )
    
    return ObjectLabel( UnderlyingQuiverObject( obj ) );
    
end );

##
@InstallMethod( CanonicalRepresentative,
        [ IsPathCategoryMorphism ],
  
  IdFunc
);

##
@InstallMethod( MorphismLabel,
          [ IsPathCategoryMorphism ],
  
  function ( alpha )
    local C, datum, labels;
    
    C = CapCategory( alpha );
    
    if (MorphismLength( alpha ) == 0)
      
      return @Concatenation( "id(", ObjectLabel( Source( alpha ) ), ")" );
      
    else
      
      labels = CollectEntries( List( MorphismSupport( alpha ), MorphismLabel ) );
      
      return JoinStringsWithSeparator(
                ListN( labels,
                  function ( l )
                    
                    if (l[2] == 1)
                      return l[1];
                    else
                      return @Concatenation( l[1], "^", StringGAP( l[2] ) );
                    end;
                    
                  end ), "⋅" );
      
    end;
    
end );

##
@InstallMethod( MorphismSupport,
          [ IsPathCategoryMorphism ],
  
  function ( alpha )
    local q;
    
    q = UnderlyingQuiver( CapCategory( alpha ) );
    
    return SetOfMorphisms( q )[MorphismIndices( alpha )];
    
end );

##
@InstallMethod( MorphismConstructor,
          [ IsPathCategory, IsPathCategoryObject, IsInt, IsDenseList, IsPathCategoryObject ],
  
  function ( C, source, length, support, target )
    
    #% CAP_JIT_DROP_NEXT_STATEMENT
    @Assert( 0, ForAll( support, MorphismFilter( UnderlyingQuiver( C ) ) ) );
    
    return MorphismConstructor( C,
                   source,
                   PairGAP( length, List( support, MorphismIndex ) ),
                   target );
    
end );

##
@InstallMethod( LaTeXOutput,
          [ IsPathCategoryMorphism ],
  
  function ( alpha )
    local C, datum, string, labels;
    
    C = CapCategory( alpha );
    
    datum = MorphismDatum( alpha );
    
    if (datum[1] == 0)
      
      string = @Concatenation( "id(", LaTeXOutput( Source( alpha ) ), ")" );
      
    else
      
      labels = CollectEntries( LabelsOfMorphisms( UnderlyingQuiver( C ) )[MorphismIndices( alpha )] );
      
      string = JoinStringsWithSeparator(
                  ListN( labels,
                    function ( label )
                      
                      if (label[2] == 1)
                        return label[1];
                      else
                        return @Concatenation( label[1], "^[", StringGAP( label[2] ), "]" );
                      end;
                      
                    end ), " " );
      
    end;
    
    if (ValueOption( "OnlyDatum" ) == true)
      
      return string;
      
    else
      
      return @Concatenation(
                "[", LaTeXOutput( Source( alpha ) ), "]-\\left(",
                "[", string, "]\\right)\\rightarrow",
                "[", LaTeXOutput( Target( alpha ) ), "]" );
    
    end;
    
end );

#= comment for Julia
##
@InstallMethod( AssignSetOfObjects,
        [ IsPathCategory, IsString ],
  
  function ( C, label )
    local names, func;
    
    names = LabelsOfObjects( UnderlyingQuiver( C ) );
    
    if (label == "" && ForAny( names, name -> IntGAP( name ) != fail ))
        Error( "the <label> passed to 'AssignSetOfObjects' must be a non-empty string!\n" );
    end;
    
    func =
      function ( name, o )
        
        name = @Concatenation( label, ReplacedString( name, "-", "m" ) );
        MakeReadWriteGlobal( name );
        @BindGlobal( name, o );
        return true;
        
    end;
    
    ListN( names, SetOfObjects( C ), func );
    
end );

##
@InstallMethod( AssignSetOfObjects,
        [ IsPathCategory ],
  
  function ( C )
    
    AssignSetOfObjects( C, "" );
    
end );

##
@InstallMethod( AssignSetOfGeneratingMorphisms,
        [ IsPathCategory, IsString ],
  
  function ( C, label )
    local names, morphisms, func;
    
    names = LabelsOfMorphisms( UnderlyingQuiver( C ) );
    
    if (label == "" && ForAny( names, name -> IntGAP( name ) != fail ))
        Error( "the <label> passed to 'AssignSetOfGeneratingMorphisms' must be a non-empty string!\n" );
    end;
    
    morphisms = SetOfGeneratingMorphisms( C );
    
    func =
      function ( name, m )
        
        name = @Concatenation( label, ReplacedString( name, "-", "m" ) );
        MakeReadWriteGlobal( name );
        @BindGlobal( name, m );
        return true;
        
    end;
    
    ListN( names, morphisms, func );
    
end );

##
@InstallMethod( AssignSetOfGeneratingMorphisms,
        [ IsPathCategory ],
  
  function ( C )
    
    AssignSetOfGeneratingMorphisms( C, "" );
    
end );
# =#

##
@InstallMethod( /,
        [ IsFinQuiverMorphism, IsPathCategory ],
  
  function ( mor, C )
    
    return SetOfGeneratingMorphisms( C )[MorphismIndex( mor )];
    
end );

##
@InstallMethod( /,
        [ IsString, IsPathCategory ],
  
  function ( label, C )
    local q, objs_labels, gmors_labels, p, id_mors_labels, sub_labels, gmor_label, l, m, power;
    
    q = UnderlyingQuiver( C );
    
    objs_labels = LabelsOfObjects( q );
    gmors_labels = LabelsOfMorphisms( q );
    
    p = Position( objs_labels, label );
    
    if (p != fail)
        
        return SetOfObjectsOfCategory( C )[p];
        
    end;
    
    id_mors_labels = List( objs_labels, obj -> @Concatenation( "id(", obj, ")" ) );
    
    if (label in id_mors_labels)
        
        label = label[ (4):(Length( label ) - 1) ];
        
        if (label in objs_labels)
            
            return IdentityMorphism( C, label / C );
            
        end;
        
    end;
    
    id_mors_labels = List( objs_labels, obj -> @Concatenation( "id_", obj ) );
    
    if (label in id_mors_labels)
        
        label = label[ (4):(Length( label )) ];
        
        if (label in objs_labels)
            
            return IdentityMorphism( C, label / C );
            
        end;
        
    end;
    
    p = Position( gmors_labels, label );
    
    if (p != fail)
        
        return SetOfGeneratingMorphismsOfCategory( C )[p];
        
    end;
    
    if (ForAny( [ "⋅", "*" ], s -> PositionSublist( label, s ) != fail ))
      
      sub_labels = SplitString( ReplacedString( label, "⋅", "*" ), "*" );
      
      return PreComposeList( C, List( sub_labels, sub_label -> sub_label / C ) );
      
    else
      
      p = PositionProperty( gmors_labels, gmor_label -> EndsWith( label, gmor_label ) );
      
      if (p != fail)
          
          gmor_label = gmors_labels[p];
          
          l = Length( label ) - Length( gmor_label );
          
          m = SetOfGeneratingMorphismsOfCategory( C )[p];
          
          if (l == 0)
              
              return m;
              
          else
              
              return PreCompose( C, label[(1):(l)] / C, m );
              
          end;
          
      else
          
          p = Positions( label, '^' );
          
          if (p != [ ])
              
              p = Last( p );
              
              power = IntGAP( label[(p + 1):(Length( label ))] );
              
              if (power != fail)
                    
                    label = label[(1):(p - 1)];
                    
                    p = PositionProperty( gmors_labels, gmor_label -> EndsWith( label, gmor_label ) );
                    
                    if (p != fail)
                        
                        gmor_label = gmors_labels[p];
                        
                        l = Length( label ) - Length( gmor_label );
                        
                        m = PreComposeList( C, ListWithIdenticalEntries( power, SetOfGeneratingMorphismsOfCategory( C )[p] ) );
                        
                        if (l == 0)
                            return m;
                        else
                            return PreCompose( C, label[(1):(l)] / C, m );
                        end;
                        
                    end;
                    
              end;
              
          end;
          
      end;
      
    end;
    
    Error( "the label '", label, "' can't be recognized! please try 'f*g' (or 'f⋅g') instead of 'fg'; and 'f*f' (or 'f⋅f') instead of 'f^2'\n" );
    
end );

#= comment for Julia
##
INSTALL_DOT_METHOD( IsPathCategory );
# =#

##
@InstallMethod( OppositePathCategory,
        "for a path category",
        [ IsPathCategory ],
        
  function( C )
    local quiver_op, range_category, C_op;
    
    quiver_op = OppositeQuiver( UnderlyingQuiver( C ) );
    
    if (HasRangeCategoryOfHomomorphismStructure( C ))
        C_op = PathCategory( quiver_op; range_of_HomStructure = RangeCategoryOfHomomorphismStructure( C ) );
    else
        C_op = PathCategory( quiver_op );
    end;
    
    SetOppositePathCategory( C_op, C );
    
    return C_op;
    
end );

##
@InstallMethod( CapFunctor,
        "for a path category, two lists, and a category",
        [ IsPathCategory, IsList, IsList, IsCapCategory ],
        
  function( C, imgs_of_objs, imgs_of_gmors, D )
    local F;
    
    F = CapFunctor( @Concatenation( "Functor from ", Name( C ), " -> ", Name( D ) ), C, D );
    
    AddObjectFunction( F,
      function ( obj )
        
        return imgs_of_objs[ObjectIndex( obj )];
        
    end );
    
    AddMorphismFunction( F,
      function ( F_s, mor, F_t )
        
        return PreComposeList( D, F_s, imgs_of_gmors[MorphismIndices( mor )], F_t );
        
    end );
    
    return F;
    
end );

###################
#
# Orders
#
###################

## Check whether mor_1 < mor_2

##
@InstallMethod( IsAscendingForMorphisms,
          [ IsPathCategory, IsPathCategoryMorphism, IsPathCategoryMorphism, IsString ],
  
  function ( C, mor_1, mor_2, admissible_order )
    local s_1, t_1, s_2, t_2, l_1, m_1, l_2, m_2, i, p_1, p_2, j;
    
    s_1 = ObjectIndex( Source( mor_1 ) );
    t_1 = ObjectIndex( Target( mor_1 ) );
    
    s_2 = ObjectIndex( Source( mor_2 ) );
    t_2 = ObjectIndex( Target( mor_2 ) );
    
    if (!( s_1 == s_2 && t_1 == t_2 ))
        Error( "the morphisms passed to 'Is(A/De)scendingForMorphisms' must have the same source & target!\n" );
    end;
    
    l_1 = MorphismLength( mor_1 );
    m_1 = MorphismIndices( mor_1 );
    
    l_2 = MorphismLength( mor_2 );
    m_2 = MorphismIndices( mor_2 );
    
    # left-degree-lexicographic order
    if (admissible_order == "Dp")
      
      if (l_1 != l_2)
          
          return l_1 < l_2;
          
      else
          
          i = PositionProperty( (1):(l_1), j -> m_1[j] != m_2[j] );
          
          return i != fail && m_1[i] > m_2[i];
          
      end;
      
    end;
    
    # right-degree-lexicographic order
    if (admissible_order == "dp")
      
      if (l_1 != l_2)
          
          return l_1 < l_2;
          
      else
          
          i = PositionProperty( Reversed( (1):(l_1) ), j -> m_1[j] != m_2[j] );
          
          return i != fail && m_1[l_1 - i + 1] > m_2[l_1 - i + 1];
          
      end;
      
    end;
    
    # total lexicographic order
    if (admissible_order == "t-lex")
      
      for j in (1):(NumberOfMorphisms( UnderlyingQuiver( C ) ))
          
          p_1 = Length( Positions( m_1, j ) );
          p_2 = Length( Positions( m_2, j ) );
          
          if (p_1 != p_2)
              return p_1 < p_2;
          end;
          
      end;
      
      i = PositionProperty( (1):(l_1), j -> m_1[j] != m_2[j] );
      
      return i != fail && m_1[i] > m_2[i];
      
    end;
    
    Error( "the passed admissible_order is unknown, it should be 'Dp', 'dp' or 't-lex'!\n" );
    
end );

##
@InstallMethod( IsAscendingForMorphisms,
          [ IsPathCategory, IsPathCategoryMorphism, IsPathCategoryMorphism ],
  
  function ( C, mor_1, mor_2 )
    
    return IsAscendingForMorphisms( C, mor_1, mor_2, C.admissible_order );
    
end );

##
@InstallMethod( IsAscendingForMorphisms,
          [ IsPathCategoryMorphism, IsPathCategoryMorphism ],
  
  function ( mor_1, mor_2 )
    
    return IsAscendingForMorphisms( CapCategory( mor_1 ), mor_1, mor_2 );
    
end );

## Check whether mor_1 > mor_2

##
@InstallMethod( IsDescendingForMorphisms,
          [ IsPathCategory, IsPathCategoryMorphism, IsPathCategoryMorphism, IsString ],
  
  function ( C, mor_1, mor_2, admissible_order )
    
    return IsAscendingForMorphisms( C, mor_2, mor_1, admissible_order );
    
end );

##
@InstallMethod( IsDescendingForMorphisms,
          [ IsPathCategory, IsPathCategoryMorphism, IsPathCategoryMorphism ],
  
  function ( C, mor_1, mor_2 )
    
    return IsAscendingForMorphisms( C, mor_2, mor_1 );
    
end );

##
@InstallMethod( IsDescendingForMorphisms,
          [ IsPathCategoryMorphism, IsPathCategoryMorphism ],
  
  function ( mor_1, mor_2 )
    
    return IsAscendingForMorphisms( mor_2, mor_1 );
    
end );

#######################################################
#
# Hom-Structure in Path Categories and their Quotients
#
#######################################################

##
@InstallMethod( IsFinitePathCategory,
          [ IsPathCategory ],
  
  function ( C )
    
    return HasFiniteNumberOfMacaulayMorphisms( C, [ ] );
    
end );

##
@InstallMethod( HasFiniteNumberOfMacaulayMorphisms,
          [ IsPathCategory, IsDenseList ],
  
  function ( C, monomials )
    local q, nr_objs, len, is_finite, current_mors, s, loop;
    
    q = UnderlyingQuiver( C );
    
    nr_objs = NumberOfObjects( q );
    
    monomials = List( monomials, m -> [ ObjectIndex( Source( m ) ), MorphismIndices( m ), ObjectIndex( Target( m ) ) ] );
    
    len = Maximum( @Concatenation( [ BigInt( 1 ) ], List( monomials, mono -> Length( mono[2] ) ) ) );
    
    while true
      
      # Hypothesis: the category is finite & all loops of length 'len' are divisible by the set 'monomials'
      is_finite = true;
      
      current_mors = ExternalHomsWithGivenLengthData( C, len );
      
      for s in (1):(nr_objs)
        
        for loop in current_mors[s][s]
          
          # if loop*loop is not divisible by any of the 'monomials', then our hypothesis "the category is finite" is wrong!
          if (@not ForAny( monomials, mono ->
                    ( IsEmpty( mono[2] ) && mono[1] in IndicesOfSources( q )[loop] ) ||
                    ( @not IsEmpty( mono[2] ) && PositionSublist( @Concatenation( loop, loop ), mono[2] ) != fail ) ))
              
              is_finite = false;
              
          # elif loop is not divisible by 'monomials' then our hypothesis "all loops of length 'len' are divisible
          # by one of the elements of 'monomials'" is wrong!
          elseif (@not ForAny( monomials, mono -> PositionSublist( loop, mono[2] ) != fail ))
              
              is_finite = fail;
              
          end;
          
          # if the category is not finite then break the current for-loop
          if (is_finite == false)
            break;
          end;
          
        end;
        
        # if the category is not finite then break the current for-loop
        if (is_finite == false)
            break;
        end;
        
      end;
      
      len = len + 1;
      
      if ((is_finite == false) || (is_finite == true && len > 2 * nr_objs))
          break;
      end;
      
    end;
    
    return is_finite;
    
end );

##
@InstallGlobalFunction( FpCategories_SORT_MORPHISMS_LIKE_QPA,
  
  function ( supports )
    local nr_objs, sort_function, s, t;
    
    nr_objs = Length( supports );
    
    sort_function =
      function ( m_1, m_2 )
        local l_1, l_2, i;
        
        l_1 = Length( m_1 );
        l_2 = Length( m_2 );
        
        if (l_1 != l_2)
          
          return l_1 < l_2;
          
        else
          
          i = PositionProperty( (1):(l_1), j -> m_1[j] != m_2[j] );
          
          return i != fail && m_1[i] < m_2[i];
          
        end;
        
    end;
    
    for s in (1):(nr_objs)
      for t in (1):(nr_objs)
        
        supports[s][t] = SortedList( supports[s][t], sort_function );
        
      end;
    end;
    
end );

##
@InstallMethod( MacaulayMorphisms,
          [ IsPathCategory, IsDenseList ],
  
  function ( C, monomials )
    local q, nr_objs, nr_mors, sources_mors, targets_mors, id_mons, non_id_mons, irr_objs, rel_objs, irr_mors, supports, len, homC_deg, hypothesis, homQ_len_st, homC_len_st, s, t, m;
    
    q = UnderlyingQuiver( C );
    
    nr_objs = NumberOfObjects( q );
    nr_mors = NumberOfMorphisms( q );
    
    sources_mors = IndicesOfSources( q );
    targets_mors = IndicesOfTargets( q );
    
    id_mons =     Filtered( monomials, m -> MorphismLength( m ) == 0 );
    non_id_mons = Filtered( monomials, m -> MorphismLength( m ) > 0 );
   
    # relevant and irrelevant objects
    irr_objs = List( id_mons, m -> ObjectIndex( Source( m ) ) );
    rel_objs = Difference( (1):(nr_objs), irr_objs );
    
    irr_mors = PositionsProperty( (1):(nr_mors), i -> sources_mors[i] in irr_objs || targets_mors[i] in irr_objs );
    
    non_id_mons = List( non_id_mons, MorphismIndices );
    
    supports = List( (1):(nr_objs), s -> List( (1):(nr_objs), t -> [ ] ) );
    
    len = BigInt( 0 );
    
    while true
      
      homC_deg = ExternalHomsWithGivenLengthData( C, len );
      
      # Hypothesis: all morphisms of length 'len' are multiples of 'monomials'
      hypothesis = true;
      
      for s in rel_objs
        for t in rel_objs
          
          homQ_len_st = [ ];
          
          homC_len_st = Filtered( homC_deg[s][t], mor -> @not ForAny( irr_mors, i -> i in mor ) );
          
          for m in homC_len_st
              
              if (ForAll( non_id_mons, datum -> PositionSublist( m, datum ) == fail ))
                  
                  Add( homQ_len_st, m );
                  
                  hypothesis = false;
                  
              end;
              
          end;
          
          supports[s][t] = @Concatenation( homQ_len_st, supports[s][t] );
          
        end;
      end;
      
      len = len + 1;
      
      if (hypothesis)
          break;
      end;
      
    end;
    
    if (C.admissible_order == "dp")
        
        FpCategories_SORT_MORPHISMS_LIKE_QPA( supports );
        
    end;
    
    return LazyHList( (1):(nr_objs), s ->
                   LazyHList( (1):(nr_objs), t ->
                           List( supports[s][t], supp ->
                                 MorphismConstructor( C,
                                         SetOfObjects( C )[s],
                                         PairGAP( Length( supp ), List( supp, BigInt ) ),
                                         SetOfObjects( C )[t] ) ) ) );
    
end );

##
@InstallMethod( MacaulayMorphisms,
          [ IsPathCategory ],
  
  function ( C )
    
    return MacaulayMorphisms( C, [ ] );
    
end );

##
@InstallMethod( ExternalHoms,
          [ IsPathCategory ],
  
  function ( C )
    
    return MacaulayMorphisms( C );
    
end );

##
@InstallMethod( DatumOfCellAsEvaluatableString,
        [ IsPathCategoryMorphism, IsList ],
        
  function( mor, list_of_evaluatable_strings )
    local datum;
    
    datum = MorphismDatum( mor );
    
    return @Concatenation( "PairGAP( ", StringGAP( datum[1] ), ", ", StringGAP( datum[2] ), " )" );
    
end );

##
@InstallMethod( ExtendFunctorToFpCategoryData,
        "for a path category, a pair of functions, and a category",
        [ IsPathCategory, IsList, IsCapCategory ],
        
  function( PQ, pair_of_funcs, category )
    local Q, functor_on_objects, functor_on_morphisms,
          extended_functor_on_objects, extended_functor_on_morphisms;
    
    Q = UnderlyingQuiver( PQ );
    
    functor_on_objects = pair_of_funcs[1];
    functor_on_morphisms = pair_of_funcs[2];
    
    ## the code below is the doctrine-specific ur-algorithm for categories
    
    extended_functor_on_objects =
      function( objPQ )
        local objQ;
        
        objQ = ObjectDatum( PQ, objPQ );
        
        return functor_on_objects( objQ );
        
    end;
    
    extended_functor_on_morphisms =
      function( source, morPQ, target )
        local s, t, morsQ;
        
        s = ObjectDatum( PQ, Source( morPQ ) );
        t = ObjectDatum( PQ, Range( morPQ ) );
        
        morsQ = MorphismDatum( PQ, morPQ )[2];
        
        return PreComposeList( category,
                       source,
                       List( morsQ, morQ -> functor_on_morphisms( morQ ) ),
                       target );
        
    end;
    
    return Triple( PQ,
                   PairGAP( extended_functor_on_objects, extended_functor_on_morphisms ),
                   category );
    
end );

##
@InstallMethod( ExtendContravariantFunctorToFpCategoryData,
        "for a path category, a pair of functions, and a category",
        [ IsPathCategory, IsList, IsCapCategory ],
        
  function( PQ, pair_of_funcs, category )
    local Q, functor_on_objects, functor_on_morphisms,
          extended_functor_on_objects, extended_functor_on_morphisms;
    
    Q = UnderlyingQuiver( PQ );
    
    functor_on_objects = pair_of_funcs[1];
    functor_on_morphisms = pair_of_funcs[2];
    
    ## the code below is the doctrine-specific ur-algorithm for categories
    
    extended_functor_on_objects =
      function( objPQ )
        local objQ;
        
        objQ = ObjectDatum( PQ, objPQ );
        
        return functor_on_objects( objQ );
        
    end;
    
    extended_functor_on_morphisms =
      function( source, morPQ, target )
        local s, t, morsQ;
        
        s = ObjectDatum( PQ, Source( morPQ ) );
        t = ObjectDatum( PQ, Range( morPQ ) );
        
        morsQ = MorphismDatum( PQ, morPQ )[2];
        
        return PostComposeList( category,
                       source,
                       List( morsQ, morQ -> functor_on_morphisms( morQ ) ),
                       target );
        
    end;
    
    return Triple( PQ,
                   PairGAP( extended_functor_on_objects, extended_functor_on_morphisms ),
                   category );
    
end );

##
@InstallMethod( DecompositionIndicesOfAllMorphismsFromHomStructure,
        "for a path category",
        [ IsPathCategory ],
        
  function( C )
    local objs;
    
    if (!(HasIsFiniteCategory( C ) && IsFiniteCategory( C )))
        TryNextMethod( );
    end;
    
    objs = SetOfObjects( C );
    
    return List( objs, t ->
                 List( objs, s ->
                       List( MorphismsOfExternalHom( C, s, t ), mor -> List( MorphismIndices( mor ), i -> -1 + i ) ) ) );
    
end );

##
@InstallMethod( RelationsAmongGeneratingMorphisms,
        "for a path category",
        [ IsPathCategory ],
        
  function( C )
    
    return [ ];
    
end );

##
@InstallMethod( CategoryFromNerveData,
        "for a path category",
        [ IsPathCategory ],
        
  function( C )
    
    if (@not IsFiniteCategory( C ))
        TryNextMethod( );
    end;
    
    return CategoryFromNerveData(
                   @rec( name = Name( C ),
                        nerve_data = NerveTruncatedInDegree2Data( C ),
                        indices_of_generating_morphisms = IndicesOfGeneratingMorphismsFromHomStructure( C ),
                        decomposition_of_all_morphisms = DecompositionIndicesOfAllMorphismsFromHomStructure( C ),
                        relations = RelationsAmongGeneratingMorphisms( C ),
                        labels = [ List( SetOfObjects( C ), ObjectLabel ), List( SetOfGeneratingMorphisms( C ), MorphismLabel ) ],
                        properties = ListKnownCategoricalProperties( C ) ) );
    
end );

##
@InstallMethod( DataTablesOfCategory,
        "for a path category",
        [ IsPathCategory ],
        
  function( C )
    
    return DataTablesOfCategory( CategoryFromNerveData( C; FinalizeCategory = true ) );
    
end );

##
@InstallGlobalFunction( CAP_INTERNAL_EXTRACT_STRING_OF_PATH,
  function ( q, path )
    local str;
    
    str = ViewString( path );
    
    return str[(1):(PositionSublist( str, ":" ) - Length( q.colors.other ) - 1)];
    
end );

###################
#
# View Methods
#
###################

##
@InstallMethod( ViewString,
          [ IsPathCategoryObject ],
  
  function ( obj )
    
    return ViewString( UnderlyingQuiverObject( obj ) );
    
end );

##
@InstallMethod( StringGAP,
          [ IsPathCategoryObject ],
  
  ViewString );

##
@InstallMethod( DisplayString,
          [ IsPathCategoryObject ],
  
  function ( o )
    
    return @Concatenation( ViewString( o ), "\n" );
    
end );

##
@InstallMethod( ViewString,
          [ IsPathCategoryMorphism ],
  
  function ( alpha )
    local colors;
    
    colors = UnderlyingQuiver( CapCategory( alpha ) ).colors;
    
    return @Concatenation(
              colors.mor,
              MorphismLabel( alpha ),
              colors.reset,
              colors.other,
              ":",
              ViewString( Source( alpha ) ),
              colors.other,
              " → ",
              ViewString( Target( alpha ) ) );

end );

##
@InstallMethod( StringGAP,
          [ IsPathCategoryMorphism ],
  
  ViewString );

##
@InstallMethod( DisplayString,
          [ IsPathCategoryMorphism ],
  
  function ( m )
    
    return @Concatenation( ViewString( m ), "\n" );
    
end );
