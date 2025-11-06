# SPDX-License-Identifier: GPL-2.0-or-later
# FpCategories: Finitely presented categories by generating quivers and relations
#
# Implementations
#
@BindGlobal( "PARSE_CAP_QUIVER_DATUM_FROM_STRING",
  function ( data_string )
    local data, p, name, objs, min, max, mors, q_datum, g, s, t, mor;
    
    data = data_string;
    
    data = ReplacedString( data, "→", "->" );
    
    p = PositionSublist( data, "(" );
    
    if (p != fail && p > 1)
        name = data[ (1):(p-1) ];
    else
        Error( "please use the format \"q(A,B)[x:A->B,y:B->A]\"\n" );
    end;
    
    data = data[ (p + 1):(Length( data ) - 1) ];
    
    data = ReplacedString( data, ")[", "|" );
    data = SplitString( data, "|" );
    
    if (IntGAP( data[1] ) != fail)
        objs = List( (1):(IntGAP( data[1] )), StringGAP );
    elseif (PositionSublist( data[1], ".." ) != fail)
        data[1] = ReplacedString( data[1], " ", "" );
        objs = SplitString( ReplacedString( data[1], "..", "," ), "," );
        min = IntGAP( objs[1] );
        max = IntGAP( objs[2] );
        objs = List( (min):(max ), StringGAP );
    else
        objs = SplitString( data[1], "," );
    end;
    
    @Assert( 0, Length( data ) in [ 1, 2 ] );
    
    if (( Length( data ) == 2 ) && ( data[2] != "" ))
        mors = SplitString( data[2], "," );
    else
        mors = [ ];
    end;
    
    q_datum = [ ];
    
    Add( q_datum, name );
    Add( q_datum, [ Length( objs ), objs ] );
    Add( q_datum, [ Length( mors ), [ ], [ ], [ ] ] );
    
    for mor in mors
        
        g = SplitString( ReplacedString( mor, "->", ":" ), ":" );
        
        if (Length( g ) != 3)
            Error( "the morphism \"", mor, "\" has an unexpected format!\n" );
        end;
        
        s = Position( objs, g[2] );
        t = Position( objs, g[3] );
        
        if (s == fail)
          Error( "the given source label \"", g[2], "\" doesn't belong the list of object labels!\n" );
        elseif (t == fail)
          Error( "the given target label \"", g[3], "\" doesn't belong the list of object labels!\n" );
        end;
        
        Add( q_datum[3][2], s ); # sources
        Add( q_datum[3][3], t ); # targets
        Add( q_datum[3][4], g[1] ); # labels
        
    end;
    
    return q_datum;
    
end );

##
@InstallMethod( RandomFinQuiver,
          [ IsInt, IsInt, IsBool ],
          
  function ( nr_objs, nr_gmors, is_cyclic )
    local datum, mors, labels, latex, s, t, p, j, triple;
    
    datum = [ "q", [ ], [ ] ];
    
    Add( datum[2], nr_objs );
    Add( datum[2], List( (1):(nr_objs), j -> @Concatenation( "v", StringGAP( j ) ) ) );
    Add( datum[2], List( (1):(nr_objs), j -> @Concatenation( "v_[", StringGAP( j ), "]" ) ) );
    
    Add( datum[3], nr_gmors );
    mors = [ ];
    labels = [ ];
    latex = [ ];
    
    for j in (1):(nr_gmors)
        
        if (is_cyclic == true)
            
            s = Random( (1):(nr_objs) );
            
            if (j == 1)
                t = s;
            else
                t = Random( (1):(nr_objs) );
            end;
            
        else
            
            s = Random( (1):(nr_objs - 1) );
            t = Random( (s + 1):(nr_objs) );
            
        end;
        
        p = Length( PositionsProperty( mors, triple -> triple[1] == s && triple[2] == t ) );
        
        Add( mors, [ s, t, p + 1 ] );
        
    end;
    
    mors = SortedList( mors );
    
    for triple in mors
      
      s = triple[1];
      t = triple[2];
      p = triple[3];
      
      if (s != t)
        Add( labels, @Concatenation( "m", StringGAP( s ), "_", StringGAP( t ), "_", StringGAP( p ) ) );
        Add( latex, @Concatenation( "m_[", StringGAP( s ), ",", StringGAP( t ), ",", StringGAP( p ), "]" ) );
      else
        Add( labels, @Concatenation( "l", StringGAP( s ), "_", StringGAP( p ) ) );
        Add( latex, @Concatenation( "l_[", StringGAP( s ), ",", StringGAP( p ), "]" ) );
      end;
      
    end;
    
    Add( datum[3], List( mors, triple -> triple[1] ) );
    Add( datum[3], List( mors, triple -> triple[2] ) );
    Add( datum[3], labels );
    Add( datum[3], latex );
    
    return FinQuiver( datum );
    
end );

##
@InstallMethod( RandomFinQuiver,
          [ IsInt, IsInt ],
          
  ( nr_objs, nr_gmors ) -> RandomFinQuiver( nr_objs, nr_gmors, false ) );

##
@InstallMethod( FinQuiver,
        [ IsList ],
        
  @FunctionWithNamedArguments(
  [
    [ "colors", false ],
    [ "name", fail ],
  ],
  function ( CAP_NAMED_ARGUMENTS, q_datum )
    local nr_gmors, q_name, q;
    
    # for Julia
    nr_gmors = q_datum[3][1];
    
    if (name == fail)
      
      q_name = @Concatenation(
                    "FinQuiver( \"",
                    q_datum[1],
                    "(",
                    JoinStringsWithSeparator( q_datum[2][2], "," ),
                    ")[",
                    JoinStringsWithSeparator( List( (1):(nr_gmors),
                        j -> @Concatenation( q_datum[3][4][j], ":", q_datum[2][2][q_datum[3][2][j]],"→",q_datum[2][2][q_datum[3][3][j]] ) ), "," ),
                    "]\" )" );
      
    else
      
      q_name = name;
      
    end;
    
    q = CreateCapCategoryWithDataTypes( q_name,
                 IsFinQuiver,
                 IsFinQuiverObject,
                 IsFinQuiverMorphism,
                 IsCapCategoryTwoCell,
                 IsBigInt,
                 IsBigInt,
                 fail
                ; overhead = false );
    
    if (colors == true)
        q.colors = @rec( obj = "\033[34m", mor = "\033[32m", other = "\033[31m", reset = "\033[0m" );
    else
        q.colors = @rec( obj = "", mor = "", other = "", reset = "" );
    end;
    
    q.category_as_first_argument = true;
    
    SetQuiverDatum( q, q_datum );
    SetQuiverName( q, q_datum[1] );
    SetNumberOfObjects( q, q_datum[2][1] );
    SetLabelsOfObjects( q, q_datum[2][2] );
    
    if (Length( q_datum[2] ) >= 3)
        SetLaTeXStringsOfObjects( q, q_datum[2][3] );
    end;
    
    SetNumberOfMorphisms( q, q_datum[3][1] );
    SetIndicesOfSources( q, q_datum[3][2] );
    SetIndicesOfTargets( q, q_datum[3][3] );
    SetLabelsOfMorphisms( q, q_datum[3][4] );
    
    if (Length( q_datum[3] ) >= 5)
        SetLaTeXStringsOfMorphisms( q, q_datum[3][5] );
    end;
    
    ##
    AddSetOfObjectsOfCategory( q,
      function ( q )
        
        return List( (1):(NumberOfObjects( q )), i ->
                     CreateCapCategoryObjectWithAttributes( q,
                             ObjectIndex, i ) );
        
    end );
    
    ##
    AddObjectConstructor( q,
      function ( q, i )
        
        return SetOfObjectsOfCategory( q )[i];
        
    end );
    
    ##
    AddObjectDatum( q,
      function ( q, obj )
        
        return ObjectIndex( obj );
        
    end );
    
    ##
    AddIsWellDefinedForObjects( q,
      function ( q, obj )
        
        return true;
        
    end );
    
    ##
    AddIsEqualForObjects( q,
      function ( q, obj_1, obj_2 )
        
        return IsIdenticalObj( obj_1, obj_2 );
        
    end );
    
    ##
    AddSetOfMorphismsOfFiniteCategory( q,
      function ( q )
        
        return List( (1):(NumberOfMorphisms( q )), j ->
                     CreateCapCategoryMorphismWithAttributes( q,
                             SetOfObjects( q )[IndicesOfSources( q )[j]],
                             SetOfObjects( q )[IndicesOfTargets( q )[j]],
                             MorphismIndex, j ) );
        
    end );
    
    ##
    AddSetOfGeneratingMorphismsOfCategory( q,
      function ( q )
        
        return SetOfMorphismsOfFiniteCategory( q );
        
    end );
    
    ##
    AddMorphismConstructor( q,
      function ( q, obj_1, i, obj_2 )
        
        if (!( IndicesOfSources( q )[i] == ObjectIndex( obj_1 ) && IndicesOfTargets( q )[i] == ObjectIndex( obj_2 ) ))
            
            Error( "the arguments passed to 'MorphismConstructor' in ", Name( q ), " are not consistent!\n" );
            
        end;
        
        return SetOfMorphismsOfFiniteCategory( q )[i];
        
    end );
    
    ##
    AddMorphismDatum( q,
      function ( q, mor )
        
        return MorphismIndex( mor );
        
    end );
    
    ##
    AddIsWellDefinedForMorphisms( q,
      function ( q, mor )
        
        return true;
        
    end );
    
    ##
    AddIsEqualForMorphisms( q,
      function ( q, mor_1, mor_2 )
        
        return IsIdenticalObj( mor_1, mor_2 );
        
    end );
    
    ##
    AddIsCongruentForMorphisms( q,
      function ( q, mor_1, mor_2 )
        
        return IsIdenticalObj( mor_1, mor_2 );
        
    end );
    
    ##
    AddMorphismsOfExternalHom( q,
      function ( q, obj_1, obj_2 )
        local s, t;
        
        s = ObjectIndex( obj_1 );
        t = ObjectIndex( obj_2 );
        
        return ExternalHoms( q )[s][t];
        
    end );
    
    Finalize( q );
    
    return q;
    
end ) );

##
@InstallMethod( FinQuiver,
          [ IsString ],
          
  @FunctionWithNamedArguments(
  [
    [ "colors", false ],
    [ "name", fail ],
  ],
  function ( CAP_NAMED_ARGUMENTS, q_datum )
    
    q_datum = PARSE_CAP_QUIVER_DATUM_FROM_STRING( q_datum );
    
    return FinQuiver( q_datum; colors = colors, name = name );
    
end ) );

##
@InstallMethod( OppositeQuiver,
          [ IsFinQuiver ],
  
  function ( q )
    local q_op;
    
    q_op =
      FinQuiver(
         @NTupleGAP( 3,
           @Concatenation( QuiverName( q ), "_op" ),
           @NTupleGAP( 3,
             NumberOfObjects( q ),
             LabelsOfObjects( q ),
             LaTeXStringsOfObjects( q ) ),
           @NTupleGAP( 5,
             NumberOfMorphisms( q ),
             IndicesOfTargets( q ),
             IndicesOfSources( q ),
             LabelsOfMorphisms( q ),
             LaTeXStringsOfMorphisms( q ) ) ); colors = q.colors );
    
    SetOppositeQuiver( q_op, q );
    
    return q_op;
    
end );

##
@InstallMethod( TensorProductOfFinQuivers,
          [ IsFinQuiver, IsFinQuiver ],
  
  function ( q1, q2 )
    local nr_objs, labels_objs, latex_strings_objs, nr_gmors, labels_gmors, latex_strings_gmors, sources_gmors, targets_gmors;
    
    nr_objs = NumberOfObjects( q1 ) * NumberOfObjects( q2 );
    
    labels_objs  = @Concatenation( List( LabelsOfObjects( q1 ), l -> List( LabelsOfObjects( q2 ), r -> @Concatenation( l, "⊗", r ) ) ) );
    
    latex_strings_objs = @Concatenation( List( LaTeXStringsOfObjects( q1 ), l -> List( LaTeXStringsOfObjects( q2 ), r -> @Concatenation( l, "\\otimes ", r ) ) ) );
    
    nr_gmors = NumberOfObjects( q1 ) * NumberOfMorphisms( q2 ) + NumberOfMorphisms( q1 ) * NumberOfObjects( q2 );
    
    labels_gmors =
        @Concatenation(
            [ @Concatenation( List( LabelsOfObjects( q1 ), l -> List( LabelsOfMorphisms( q2 ), r -> @Concatenation( l, "⊗", r ) ) ) ),
              @Concatenation( List( LabelsOfMorphisms( q1 ), l -> List( LabelsOfObjects( q2 ), r -> @Concatenation( l, "⊗", r ) ) ) ) ] );
    
    latex_strings_gmors =
        @Concatenation(
            [ @Concatenation( List( LaTeXStringsOfObjects( q1 ), l -> List( LaTeXStringsOfMorphisms( q2 ), r -> @Concatenation( l, "\\otimes ", r ) ) ) ),
              @Concatenation( List( LaTeXStringsOfMorphisms( q1 ), l -> List( LaTeXStringsOfObjects( q2 ), r -> @Concatenation( l, "\\otimes ", r ) ) ) ) ] );

    sources_gmors =
        @Concatenation(
            [ @Concatenation( List( (1):(NumberOfObjects( q1 )), l -> List( IndicesOfSources( q2 ), r -> (l-1) * NumberOfObjects( q2 ) + r ) ) ),
              @Concatenation( List( IndicesOfSources( q1 ), l -> List( (1):(NumberOfObjects( q2 )), r -> (l-1) * NumberOfObjects( q2 ) + r ) ) ) ] );
    
    targets_gmors =
        @Concatenation(
            [ @Concatenation( List( (1):(NumberOfObjects( q1 )), l -> List( IndicesOfTargets( q2 ), r -> (l-1) * NumberOfObjects( q2 ) + r ) ) ),
              @Concatenation( List( IndicesOfTargets( q1 ), l -> List( (1):(NumberOfObjects( q2 )), r -> (l-1) * NumberOfObjects( q2 ) + r ) ) ) ] );
    
    return FinQuiver(
               @NTupleGAP( 3,
                 @Concatenation( QuiverName( q1 ), "⊗", QuiverName( q2 ) ),
                 @NTupleGAP( 3,
                   nr_objs,
                   labels_objs,
                   latex_strings_objs ),
                 @NTupleGAP( 5,
                   nr_gmors,
                   sources_gmors,
                   targets_gmors,
                   labels_gmors,
                   latex_strings_gmors ) ); colors = q1.colors );

end );

##
@InstallMethod( SetOfObjects,
          [ IsFinQuiver ],
  
  function ( q )
    
    return SetOfObjectsOfCategory( q );
    
end );

##
@InstallMethod( SetOfMorphisms,
          [ IsFinQuiver ],
  
  function ( q )
    
    return SetOfMorphismsOfFiniteCategory( q );
    
end );

##
@InstallMethod( LaTeXStringsOfObjects,
          [ IsFinQuiver ],
  function ( q )
    
    return LabelsOfObjects( q );
    
end );

##
@InstallMethod( LaTeXStringsOfMorphisms,
          [ IsFinQuiver ],
  function ( q )
    
    return LabelsOfMorphisms( q );
    
end );

##
@InstallMethod( IndicesPairsOfCompatibleMorphisms,
          [ IsFinQuiver ],
  
  function ( q )
    local sources, targets;
    
    sources = IndicesOfSources( q );
    targets = IndicesOfTargets( q );
    
    return @Concatenation( List( (1):(NumberOfMorphisms( q )), i -> List( Filtered( (1):(NumberOfMorphisms( q )), j -> targets[i] == sources[j] ), j -> [ i, j ] ) ) );
    
end );

##
@InstallMethod( ExternalHoms,
          [ IsFinQuiver ],
  
  function ( q )
    local sources, targets;
    
    sources = IndicesOfSources( q );
    targets = IndicesOfTargets( q );
    
    return List( (1):(NumberOfObjects( q )),
              s -> List( (1):(NumberOfObjects( q )),
                t -> SetOfMorphisms( q )[Filtered( (1):(NumberOfMorphisms( q )), j -> s == sources[j] && t == targets[j] )] ) );
    
end );

##
@InstallMethod( /,
        [ IsString, IsFinQuiver ],
        
  function ( label, q )
    local i;
    
    i = Position( LabelsOfObjects( q ), label );
    
    if (i != fail)
        return SetOfObjects( q )[i];
    end;
    
    i = Position( LabelsOfMorphisms( q ), label );
    
    if (i != fail)
        return SetOfMorphisms( q )[i];
    end;
    
    Error( "the label '", label, "' can't be recognized!\n" );
    
end );

#= comment for Julia
##
INSTALL_DOT_METHOD( IsFinQuiver );
# =#

##
@InstallMethod( ObjectLabel,
          [ IsFinQuiverObject ],
  
  obj -> LabelsOfObjects( CapCategory( obj ) )[ObjectIndex( obj )]
);

##
@InstallMethod( LaTeXOutput,
          [ IsFinQuiverObject ],
  
  obj -> LaTeXStringsOfObjects( CapCategory( obj ) )[ObjectIndex( obj )]
);

##
@InstallMethod( MorphismLabel,
          [ IsFinQuiverMorphism ],
  
  mor -> LabelsOfMorphisms( CapCategory( mor ) )[MorphismIndex( mor )]
);

##
@InstallMethod( LaTeXOutput,
          [ IsFinQuiverMorphism ],
  
  function ( mor )
    local str;
    
    str = LaTeXStringsOfMorphisms( CapCategory( mor ) )[MorphismIndex( mor )];
    
    if (ValueOption( "OnlyDatum" ) == true)
      
      return str;
      
    else
      
      return @Concatenation(
                "[", LaTeXOutput( Source( mor ) ), "]-\\left(",
                "[", str, "]\\right)\\rightarrow",
                "[", LaTeXOutput( Target( mor ) ), "]" );
      
    end;
    
end );

###################
#
# View Methods
#
###################

##
@InstallMethod( ViewString,
          [ IsFinQuiverObject ],
  
  function ( obj )
    local colors;
    
    colors = CapCategory( obj ).colors;
    
    return @Concatenation( colors.obj, "(", ObjectLabel( obj ), ")", colors.reset );
    
end );

##
@InstallMethod( DisplayString,
          [ IsFinQuiverObject ],
        
  function( obj )
    
    return @Concatenation( ViewString( obj ), "\n" );
    
end );

##
@InstallMethod( StringGAP,
          [ IsFinQuiverObject ],
  
  ViewString );

##
@InstallMethod( ViewString,
          [ IsFinQuiverMorphism ],
  
  function ( mor )
    local colors;
    
    colors = CapCategory( mor ).colors;
    
    return @Concatenation(
              colors.mor,
              MorphismLabel( mor ),
              colors.reset,
              colors.other,
              ":",
              ViewString( Source( mor ) ),
              colors.other,
              " → ",
              ViewString( Target( mor ) ) );
    
end );

##
@InstallMethod( DisplayString,
          [ IsFinQuiverMorphism ],
  
  function( mor )
    
    return @Concatenation( ViewString( mor ), "\n" );
    
end );

##
@InstallMethod( StringGAP,
          [ IsFinQuiverMorphism ],
  
  ViewString );
