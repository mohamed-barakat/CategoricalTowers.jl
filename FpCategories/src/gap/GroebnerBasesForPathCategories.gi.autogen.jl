# SPDX-License-Identifier: GPL-2.0-or-later
# FpCategories: Finitely presented categories by generating quivers and relations
#
# Implementations
#


##########################
#
# Categorical Relations
#
##########################

## For each g in G, we must have g[1] > g[2], otherwise it might never terminate.
## The output is a morphism r < f such that r & f are congruent modulo G

##
@InstallMethod( ReductionOfMorphism,
          [ IsPathCategory, IsPathCategoryMorphism, IsDenseList ],
          
  function ( C, f, G )
    local Q, G_datum, predicate, func, initial_value;
    
    Q = UnderlyingQuiver( C );
    
    G_datum = List( G, g -> PairGAP( MorphismLength( g[1] ), MorphismIndices( g[1] ) ) );
    
    predicate = ( mor_1, mor_2 ) -> MorphismIndices( mor_1 ) == MorphismIndices( mor_2 );
    
    func =
      function ( f )
        local f_datum, i, g_datum, j, u, v;
        
        f_datum = PairGAP( MorphismLength( f ), MorphismIndices( f ) );
        
        i = PositionProperty( G_datum, g_datum -> f_datum[1] >= g_datum[1] && PositionSublist( f_datum[2], g_datum[2] ) != fail );
        
        if (i == fail)
          
          return f;
          
        else
          
          g_datum = G_datum[i];
          
          j = PositionSublist( f_datum[2], g_datum[2] );
          
          return MorphismConstructor( C,
                         Source( f ),
                         PairGAP( First( f_datum ) - First( g_datum ) + MorphismLength( G[i][2] ),
                               @Concatenation( f_datum[2][ (1):(j - 1) ], MorphismIndices( G[i][2] ), f_datum[2][ (j + First( g_datum )):(First( f_datum )) ] ) ),
                         Target( f ) );
          
        end;
        
    end;
    
    initial_value = f;
    
    return CapFixpoint( predicate, func, initial_value );
    
end );

##
##  all pairs [ [l_g, r_g], [l_h, r_h] ] which satisfy l_g*g*r_g == l_h*h*r_h
##
@InstallMethod( OverlappingCoefficients,
          [ IsPathCategory, IsPathCategoryMorphism, IsPathCategoryMorphism ],
  
  function ( C, f, g )
    local Q, l_f, i_f, l_g, i_g, pos, overlaps_on_left_of_g, overlaps_on_right_of_g, inner_overlaps;
    
    Q = UnderlyingQuiver( C );
    
    l_f = MorphismLength( f );
    i_f = MorphismIndices( f );
    
    l_g = MorphismLength( g );
    i_g = MorphismIndices( g );
    
    pos = PositionsProperty( (1):(Minimum( l_f, l_g )),
              i ->  (i != l_f || i != l_g) && i_g[(l_g - i + 1):(l_g)] == i_f[(1):(i)] );
    
    overlaps_on_left_of_g =
      List( pos,
        i ->  PairGAP(
                PairGAP(
                  MorphismConstructor( C, Source( g ), PairGAP( l_g - i, i_g[(1):(l_g - i)] ), Source( f ) ),
                  IdentityMorphism( C, Target( f ) ) ),
                PairGAP(
                  IdentityMorphism( C, Source( g ) ),
                  MorphismConstructor( C, Target( g ), PairGAP( l_f - i, i_f[(i + 1):(l_f)] ), Target( f ) ) ) ) );
    
    pos = PositionsProperty( (1):(Minimum( l_f, l_g )),
              i ->  ( i != l_f || i != l_g )
                    && i_f[(l_f - i + 1):(l_f)] == i_g[(1):(i)] );
    
    overlaps_on_right_of_g =
      List( pos,
        i -> PairGAP(
                PairGAP(
                  IdentityMorphism( C, Source( f ) ),
                  MorphismConstructor( C, Target( f ), PairGAP( l_g - i, i_g[(i + 1):(l_g)] ), Target( g ) ) ),
                PairGAP(
                  MorphismConstructor( C, Source( f ), PairGAP( l_f - i, i_f[(1):(l_f - i)] ), Source( g ) ),
                  IdentityMorphism( C, Target( g ) ) ) ) );
    
    if (l_f >= l_g)
      
      inner_overlaps =
            List( PositionsOfSublist( i_f, i_g ),
              i -> PairGAP(
                    PairGAP(
                      IdentityMorphism( C, Source( f ) ),
                      IdentityMorphism( C, Target( f ) ) ),
                    PairGAP(
                      MorphismConstructor( C, Source( f ), PairGAP( i - 1, i_f[(1):(i - 1)] ), Source( g ) ),
                      MorphismConstructor( C, Target( g ), PairGAP( l_f - i - l_g + 1, i_f[(i + l_g):(l_f)] ), Target( f ) ) ) ) );
      
      return @Concatenation( inner_overlaps, overlaps_on_left_of_g, overlaps_on_right_of_g );
      
    else
      
      inner_overlaps =
            List( PositionsOfSublist( i_g, i_f ),
              i -> PairGAP(
                    PairGAP(
                      MorphismConstructor( C, Source( g ), PairGAP( i - 1, i_g[(1):(i - 1)] ), Source( f ) ),
                      MorphismConstructor( C, Target( f ), PairGAP( l_g - i - l_f + 1, i_g[(i + l_f):(l_g)] ), Target( g ) ) ),
                    PairGAP(
                      IdentityMorphism( C, Source( g ) ),
                      IdentityMorphism( C, Target( g ) ) ) ) );
      
      return @Concatenation( inner_overlaps, overlaps_on_left_of_g, overlaps_on_right_of_g );
      
    end;
    
end );

##
@InstallMethod( NewRelation,
          [ IsPathCategory, IsDenseList, IsDenseList, IsDenseList ],
  
  function ( C, rel_1, rel_2, overlap_coeffs )
    local m1, m2;
    
    #@Assert( 0, IsEqualForMorphisms( C,
    #                PreComposeList( C, [ overlap_coeffs[1][1], rel_1[1], overlap_coeffs[1][2] ] ),
    #                PreComposeList( C, [ overlap_coeffs[2][1], rel_2[1], overlap_coeffs[2][2] ] ) ) );
    
    m1 = PreComposeList( C, [ overlap_coeffs[1][1], rel_1[2], overlap_coeffs[1][2] ] );
    m2 = PreComposeList( C, [ overlap_coeffs[2][1], rel_2[2], overlap_coeffs[2][2] ] );
    
    if (IsAscendingForMorphisms( C, m1, m2 ))
        return PairGAP( m2, m1 );
    else
        return PairGAP( m1, m2 );
    end;
    
end );

##
@InstallMethod( GroebnerBasis,
          [ IsPathCategory, IsDenseList ],
          
  function ( C, relations )
    local gb, indices, rels, i, g1, g2, new_rels;
    
    gb = List( (1):(Length( relations )), i -> List( [ 1, 2 ], j -> relations[i][j] ) );
    
    for i in (1):(Length( gb ))
        gb[i] = SortedList( gb[i], ( g1, g2 ) -> IsAscendingForMorphisms( C, g2, g1 ) );
    end;
    
    indices = UnorderedTuples( (1):(Length( gb )), 2 );
    
    rels = @Concatenation( List( indices,
                              i -> List( OverlappingCoefficients( C, gb[i[1]][1], gb[i[2]][1] ),
                                overlap_coeffs -> NewRelation( C, gb[i[1]], gb[i[2]], overlap_coeffs ) ) ) );
    
    i = 1;
    
    while i <= Length( rels )
        
        if (@not IsEqualForMorphisms( C, rels[i][1], rels[i][2] ))
            
            g1 = ReductionOfMorphism( C, rels[i][1], gb );
            g2 = ReductionOfMorphism( C, rels[i][2], gb );
            
            if (@not IsEqualForMorphisms( C, g1, g2 ))
                
                if (IsAscendingForMorphisms( C, g1, g2 ))
                    Add( gb, PairGAP( g2, g1 ) );
                else
                    Add( gb, PairGAP( g1, g2 ) );
                end;
                
                indices = Cartesian( (1):(Length( gb )), [ Length( gb ) ] );
                
                new_rels =
                  @Concatenation(
                      List( indices,
                        i -> List( OverlappingCoefficients( C, gb[i[1]][1], gb[i[2]][1] ),
                                overlap_coeffs -> NewRelation( C, gb[i[1]], gb[i[2]], overlap_coeffs ) ) ) );
                
                rels = @Concatenation( rels, new_rels );
                
          end;
          
        end;
        
        i = i + 1;
        
    end;
    
    return gb;
    
end );

##
@InstallMethod( ReducedGroebnerBasisWithGivenGroebnerBasis,
          [ IsPathCategory, IsDenseList ],
  
  function ( C, gb )
    local reduced_gb, i, H, r1, r2;
    
    reduced_gb = ShallowCopy( gb );
    
    i = 1;
    
    while i <= Length( reduced_gb )
      
      H = @Concatenation( reduced_gb[ (1):(i-1) ], reduced_gb[ (i+1):(Length( reduced_gb )) ] );
      
      r1 = ReductionOfMorphism( C, reduced_gb[i][1], H );
      r2 = ReductionOfMorphism( C, reduced_gb[i][2], H );
      
      if (IsEqualForMorphisms( C, r1, r2 ))
          
          Remove( reduced_gb, i );
          
      elseif (!(IsEqualForMorphisms( C, r1, reduced_gb[i][1] )) || @not IsEqualForMorphisms( C, r2, reduced_gb[i][2] ))
          
          if (IsAscendingForMorphisms( C, r1, r2 ))
              reduced_gb[i] = [ r2, r1 ];
          else
              reduced_gb[i] = [ r1, r2 ];
          end;
          
          i = 1;
          
      else
          
          i = i + 1;
          
      end;
      
    end;
    
    return reduced_gb;
    
end );

##
@InstallMethod( ReducedGroebnerBasis,
          [ IsPathCategory, IsDenseList ],
  
  function ( C, relations )
    
    return ReducedGroebnerBasisWithGivenGroebnerBasis( C, GroebnerBasis( C, relations ) );
    
end );

