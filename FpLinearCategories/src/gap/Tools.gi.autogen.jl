# SPDX-License-Identifier: GPL-2.0-or-later
# FpLinearCategories: Finitely presented linear categories by generating quivers and relations
#
# Implementations
#

##
@InstallMethod( Dimension,
        "for a linear category with finite objects and free external homs",
        [ IsCapCategory ],
        
  function( A )
    local objects;
    
    if (!( HasIsObjectFiniteCategory( A ) && IsObjectFiniteCategory( A ) &&
              HasIsLinearCategoryOverCommutativeRingWithFinitelyGeneratedFreeExternalHoms( A ) &&
              IsLinearCategoryOverCommutativeRingWithFinitelyGeneratedFreeExternalHoms( A ) ))

        TryNextMethod( );
    end;
    
    objects = SetOfObjects( A );
    
    return Sum( objects, s -> Sum( objects, t -> ObjectDatum( HomomorphismStructureOnObjects( s, t ) ) ) );
    
end );
