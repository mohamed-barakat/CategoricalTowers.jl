
julia> true
true

```jldoctest
julia> using CAP, QuotientCategories, FpCategories

julia> C = PathCategory( FinQuiver( "q(1,2,3,4,5,6,7)[12:1->2,23:2->3,34:3->4,45:4->5,56:5->6,67:6->7,77:7->7,71:7->1]" ) );

julia> @Assert( 0, @not HasFiniteNumberOfMacaulayMorphisms( C, [ C["12*23*34*45*56*67*77*71"] ] ) )

julia> @Assert( 0, @not HasFiniteNumberOfMacaulayMorphisms( C, [ C["77"], C["12*23*34*45*56*67*77*71"] ] ) )

julia> @Assert( 0, @not HasFiniteNumberOfMacaulayMorphisms( C, [ C["12*23*34*45*56*67*71"], C["12*23*34*45*56*67*77*71"] ] ) )

julia> @Assert( 0, HasFiniteNumberOfMacaulayMorphisms( C, [ C["77"], C["12*23*34*45*56*67*71"] ] ) )

julia> @Assert( 0, HasFiniteNumberOfMacaulayMorphisms( C, [ C["77"], C["id_1"] ] ) )

julia> @Assert( 0, @not HasFiniteNumberOfMacaulayMorphisms( C, [ C["id_1"] ] ) )

julia> @Assert( 0, HasFiniteNumberOfMacaulayMorphisms( C, [ C["id_7"] ] ) )

```
