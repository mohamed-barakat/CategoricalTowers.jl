# SPDX-License-Identifier: GPL-2.0-or-later
# FpCategories: Finitely presented categories by generating quivers and relations
#
# Implementations
#

##
@InstallMethod( QuiverStringOfDigraph,
               [ IsDigraph, IsString, IsString ],
               
  function( digraph, name, mor )
    local string, vertices, label, labels, mors;
    
    string = [ name, "(" ];
    
    vertices = DigraphVertices( digraph );
    
    label =
      function( vertex )
        local str;
        
        str = StringGAP( DigraphVertexLabel( digraph, vertex ) );
        
        if (First( str ) == '(' && Last( str ) == ')')
            str = str[(2):(Length( str ) - 1)];
        end;
        
        return str;
        
    end;
    
    labels = List( vertices, label );
    
    Append( string, [ JoinStringsWithSeparator( labels, "," ), ")[" ] );
    
    mors =
      function( s )
        local targets;
        
        targets = OutNeighborsOfVertex( digraph, s );
        
        if (DuplicateFreeList( targets ) == targets)
            return List( targets, t ->
                         @Concatenation( mor, "_", StringGAP( s ), "_", StringGAP( t ), ":", labels[s], "->", labels[t] ) );
        else
            return List( (1):(Length( targets )), i ->
                         @Concatenation( mor, "_", StringGAP( s ), "_", StringGAP( targets[i] ), "_", StringGAP( i ), ":", labels[s], "->", labels[targets[i]] ) );
        end;
        
    end;
    
    Append( string, [ JoinStringsWithSeparator( @Concatenation( List( (1):(Length( vertices )), mors ) ), "," ), "]" ] );
    
    return @Concatenation( string );
    
end );
