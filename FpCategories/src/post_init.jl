
##
InstallMethod( ObjectConstructor,
          [ IsFinQuiver, IsInt ],
  function( q, i )
    return ObjectConstructor( q, BigInt(i) );
end );

##
InstallMethod( MorphismConstructor,
          [ IsFinQuiver, IsFinQuiverObject, IsInt, IsFinQuiverObject ],
  function( q, source, m, range )
    return MorphismConstructor( q, source, BigInt( m ), range );
end );

##
InstallMethod( CreateObject,
          [ IsCategoryFromNerveData, IsInt ],
  function( C, i )
    return CreateObject( C, BigInt( i ) );
end );

##
InstallMethod( CreateMorphism,
          [ IsObjectInCategoryFromNerveData, IsInt, IsObjectInCategoryFromNerveData ],
  function( source, m, range )
    return CreateMorphism( source, BigInt( m ), range );
end );

##
InstallMethod( CreateMorphism,
          [ IsCategoryFromNerveData, IsInt ],
  function( C, m )
    return CreateMorphism( C, BigInt( m ) );
end );


##
InstallMethod( /,
        [ IsInt, IsCategoryFromNerveData ],
  function( i, C )
    return CreateObject( C, i );
end );
