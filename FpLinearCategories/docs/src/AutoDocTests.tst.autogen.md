
```jldoctest AutoDocTests
julia> using MatricesForHomalg, CAP, MonoidalCategories, CartesianCategories, ToolsForCategoricalTowers, Toposes, FinSetsForCAP, QuotientCategories, FpCategories, LinearClosuresForCAP, AdditiveClosuresForCAP, LinearAlgebraForCAP, FreydCategoriesForCAP, FpLinearCategories

julia> true
true

julia> q = FinQuiver( "q(0,1,2,3)[a:0->1,b:1->3,c:0->2,d:2->3,e:3->3]" )
FinQuiver( "q(0,1,2,3)[a:0→1,b:1→3,c:0→2,d:2→3,e:3→3]" )

julia> F = PathCategory( q )
PathCategory( FinQuiver( "q(0,1,2,3)[a:0→1,b:1→3,c:0→2,d:2→3,e:3→3]" ) )

julia> k = HomalgFieldOfRationals( );

julia> kF = k[F]
Q-LinearClosure( PathCategory( FinQuiver( "q(0,1,2,3)[a:0→1,b:1→3,c:0→2,d:2→3,e:3→3]" ) ) )

julia> B = kF / [ kF.e^3 ]
Q-LinearClosure( PathCategory( FinQuiver( "q(0,1,2,3)[a:0→1,b:1→3,c:0→2,d:2→3,e:3→3]" ) ) ) / [ 1*e^3 ]

julia> data_tables = DataTablesOfLinearCategory( B );

julia> IsIdenticalObj( data_tables[1], CommutativeRingOfLinearCategory( B ) )
true

julia> data_tables[2]
FinQuiver( "q(0,1,2,3)[a:0→1,b:1→3,c:0→2,d:2→3,e:3→3]" )

julia> Perform( data_tables[3], Display )
[ [ [  ] ], [ [ 1 ] ], [ [ 3 ] ], [ [ 1, 2 ], [ 3, 4 ], [ 1, 2, 5 ], [ 3, 4, 5 ], [ 1, 2, 5, 5 ], [ 3, 4, 5, 5 ] ] ]
[ [  ], [ [  ] ], [  ], [ [ 2 ], [ 2, 5 ], [ 2, 5, 5 ] ] ]
[ [  ], [  ], [ [  ] ], [ [ 4 ], [ 4, 5 ], [ 4, 5, 5 ] ] ]
[ [  ], [  ], [  ], [ [  ], [ 5 ], [ 5, 5 ] ] ]

julia> Perform( data_tables[4], Display )
[ [ [ 1 ] ], [ [ 1, 0, 0, 0, 0, 0 ] ], [ [ 1 ] ], [ [ 0, 1, 0, 0, 0, 0 ] ],
  [ [ 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0 ],
    [ 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 1 ], [ 0, 0, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 0 ] ] ]
[ [  ], [ [ 1, 0, 0 ] ], [  ], [  ], [ [ 0, 1, 0 ], [ 0, 0, 1 ], [ 0, 0, 0 ] ] ]
[ [  ], [  ], [  ], [ [ 1, 0, 0 ] ], [ [ 0, 1, 0 ], [ 0, 0, 1 ], [ 0, 0, 0 ] ] ]
[ [  ], [  ], [  ], [  ], [ [ 0, 1, 0 ], [ 0, 0, 1 ], [ 0, 0, 0 ] ] ]

julia> A = AlgebroidFromDataTables( data_tables )
Q-algebroid( [0,1,2,3][a:0→1,b:1→3,c:0→2,d:2→3,e:3→3] ) defined by
4 objects and 5 generating morphisms

julia> Perform( SetOfObjects( A ), Display )
<(0)>
<(1)>
<(2)>
<(3)>

julia> A["0"]
<(0)>

julia> ObjectConstructor( A, 1 ) == A["0"]
true

julia> Perform( SetOfGeneratingMorphisms( A ), Display )
<1*a:(0) → (1)>
<1*b:(1) → (3)>
<1*c:(0) → (2)>
<1*d:(2) → (3)>
<1*e:(3) → (3)>

julia> HomStructure( A["0"], A["3"] )
<A row module over Q of rank 6>

julia> hom_03 = BasisOfExternalHom( A["0"], A["3"] );

julia> Perform( hom_03, Display )
<1*a⋅b:(0) → (3)>
<1*c⋅d:(0) → (3)>
<1*a⋅b⋅e:(0) → (3)>
<1*c⋅d⋅e:(0) → (3)>
<1*a⋅b⋅e^2:(0) → (3)>
<1*c⋅d⋅e^2:(0) → (3)>

julia> alpha = 2*hom_03[1] + 3*hom_03[6]
<2*a⋅b + 3*c⋅d⋅e^2:(0) → (3)>

julia> alpha == MorphismConstructor( A, A["0"], [ 2, 0, 0, 0, 0, 3 ], A["3"] )
true

julia> Display( CoefficientsList( alpha ) )
[ 2, 0, 0, 0, 0, 3 ]

julia> Display( IndicesOfSupportMorphisms( alpha ) )
[ 1, 6 ]

julia> Display( CoefficientsOfSupportMorphisms( alpha ) )
[ 2, 3 ]

julia> Perform( SupportMorphisms( alpha ), Display )
<1*a⋅b:(0) → (3)>
<1*c⋅d⋅e^2:(0) → (3)>

julia> Display( DecompositionIndicesOfMorphismInAlgebroid( alpha ) )
[ [ 2, [ 1, 2 ] ], [ 3, [ 3, 4, 5, 5 ] ] ]

julia> d = DecompositionOfMorphismInAlgebroid( alpha );

julia> d[1][1]
2

julia> Perform( d[1][2], Display )
<1*a:(0) → (1)>
<1*b:(1) → (3)>

julia> d[2][1]
3

julia> Perform( d[2][2], Display )
<1*c:(0) → (2)>
<1*d:(2) → (3)>
<1*e:(3) → (3)>
<1*e:(3) → (3)>

julia> A["a⋅b⋅e"]
<1*a⋅b⋅e:(0) → (3)>

julia> A["abe"]
<1*a⋅b⋅e:(0) → (3)>

julia> PreCompose( A["a"], A["b"] ) == A["ab"]
true

julia> add_A = AdditiveClosure( A )
AdditiveClosure( Q-algebroid( [0,1,2,3][a:0→1,b:1→3,c:0→2,d:2→3,e:3→3] )
defined by 4 objects and 5 generating morphisms )

julia> o1 =  RandomObject( add_A, [ [ 5 ], [ 1 ] ] );

julia> o2 =  RandomObject( add_A, [ [ 5 ], [ 1 ] ] );

julia> f = RandomMorphism( o1, o2, 20 );

julia> HomStructure( o1, o2, HomStructure( f ) ) == f
true

julia> IsZeroForMorphisms( PreCompose( f, WeakCokernelProjection( f ) ) )
true

julia> IsZeroForMorphisms( PreCompose( WeakKernelEmbedding( f ), f ) )
true

julia> freyd_B = FreydCategory( add_A )
Freyd( AdditiveClosure( Q-algebroid( [0,1,2,3][a:0→1,b:1→3,c:0→2,d:2→3,
e:3→3] ) defined by 4 objects and 5 generating morphisms ) )

julia> IsAbelianCategory( freyd_B )
true

julia> A_op = OppositeAlgebroid( A )
Q-algebroid( [0,1,2,3][a:1→0,b:3→1,c:2→0,d:3→2,e:3→3] ) defined by
4 objects and 5 generating morphisms

julia> A_op["ba"]
<1*b⋅a:(3) → (0)>

julia> T = DirectSum( List( SetOfObjects( A ), o -> o / add_A ) )
<An object in AdditiveClosure( Q-algebroid( [0,1,2,3][a:0→1,b:1→3,c:0→2,
d:2→3,e:3→3] ) defined by 4 objects and 5 generating morphisms ) defined
by 4 underlying objects>

julia> u = RandomMorphism( T, T, 5 );

julia> v = RandomMorphism( T, T, 5 );

julia> w = RandomMorphism( T, T, 5 );

julia> HomStructure( PreCompose( [ u, v, w ] ) ) == PreCompose( HomStructure( v ), HomStructure( u, w ) )
true

julia> AA = TensorProductOfAlgebroids( A, A )
Q-algebroid( [0⊗0,0⊗1,0⊗2,..,3⊗1,3⊗2,3⊗3][0⊗a:0⊗0→0⊗1,0⊗b:0⊗1→0⊗3,
0⊗c:0⊗0→0⊗2,..,e⊗1:3⊗1→3⊗1,e⊗2:3⊗2→3⊗2,e⊗3:3⊗3→3⊗3] ) defined
by 16 objects and 40 generating morphisms

julia> ElementaryTensor( A["0"], A["1"] )
<(0⊗1)>

julia> ElementaryTensor( A["a"], A["b"] )
<1*a⊗b:(0⊗1) → (1⊗3)>

julia> ElementaryTensor( A["0"], A["b"] )
<1*id(0)⊗b:(0⊗1) → (0⊗3)>

julia> ElementaryTensor( A["a"], A["1"] )
<1*a⊗id(1):(0⊗1) → (1⊗1)>

julia> o = AA["3⊗3"]
<(3⊗3)>

julia> AA["e⊗1"]
<1*e⊗id(1):(3⊗1) → (3⊗1)>

julia> u = RandomMorphism( o, o, 15 );

julia> v = RandomMorphism( o, o, 15 );

julia> w = RandomMorphism( o, o, 15 );

julia> HomStructure( PreCompose( [ u, v, w ] ) ) == PreCompose( HomStructure( v ), HomStructure( u, w ) )
true

```

```jldoctest AutoDocTests
julia> using MatricesForHomalg, CAP, MonoidalCategories, CartesianCategories, ToolsForCategoricalTowers, Toposes, FinSetsForCAP, QuotientCategories, FpCategories, LinearClosuresForCAP, AdditiveClosuresForCAP, LinearAlgebraForCAP, FreydCategoriesForCAP, FpLinearCategories

julia> true
true

julia> str = "q(0..5)[x:0->0,s:0->1,a:1->2,c:1->3,e:1->4,b:2->4,d:3->4,t:4->5,y:5->5]";

julia> q = FinQuiver( str )
FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,b:2→4,d:3→4,
t:4→5,y:5→5]" )

julia> k = HomalgFieldOfRationals( );

julia> C = PathCategory( q; admissible_order = "Dp" )
PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,
b:2→4,d:3→4,t:4→5,y:5→5]" ) )

julia> kC = LinearClosure( k, C )
Q-LinearClosure( PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,
c:1→3,e:1→4,b:2→4,d:3→4,t:4→5,y:5→5]" ) ) )

julia> rels = [ kC.x^10 - kC.x^5, kC.abt - kC.et, kC.y^10 - kC.y^5, kC.x^5, kC.y^5 ];

julia> Perform( rels, Display )
1*x^10 + (-1)*x^5:(0) → (0)
1*a⋅b⋅t + (-1)*e⋅t:(1) → (5)
1*y^10 + (-1)*y^5:(5) → (5)
1*x^5:(0) → (0)
1*y^5:(5) → (5)

julia> quo_kC = QuotientCategory( kC, rels )
Q-LinearClosure( PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,
c:1→3,e:1→4,b:2→4,d:3→4,t:4→5,y:5→5]" ) ) ) / [ 1*x^10 + (-1)*x^5,
1*a⋅b⋅t + (-1)*e⋅t, 1*y^10 + (-1)*y^5, ... ]

julia> HomStructure( quo_kC["0"], quo_kC["5"] )
<A row module over Q of rank 50>

julia> A = AlgebroidFromDataTables( quo_kC )
Q-algebroid( [0,1,2,3,4,5][x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,b:2→4,d:3→4,
t:4→5,y:5→5] ) defined by 6 objects and 9 generating morphisms

julia> HomStructure( A["0"], A["5"] )
<A row module over Q of rank 50>

julia> quo_C = C / [ [ C.x^10, C.x^5 ], [ C.abt, C.et ], [ C.y^10, C.y^5 ] ]
PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,
b:2→4,d:3→4,t:4→5,y:5→5]" ) ) / [ x^10 == x^5, a⋅b⋅t == e⋅t, y^10 == y^5 ]

julia> k_quo_C = k[quo_C]
Q-LinearClosure( PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,
c:1→3,e:1→4,b:2→4,d:3→4,t:4→5,y:5→5]" ) ) / [ x^10 == x^5, a⋅b⋅t == e⋅t,
y^10 == y^5 ] )

julia> quo_k_quo_C = k_quo_C / [ k_quo_C.x^5, k_quo_C.y^5 ]
Q-LinearClosure( PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,
c:1→3, e:1→4,b:2→4,d:3→4,t:4→5,y:5→5]" ) ) / [ x^10 == x^5, a⋅b⋅t == e⋅t,
y^10 == y^5 ] ) / [ 1*[x^5], 1*[y^5] ]

julia> HomStructure( quo_k_quo_C["0"], quo_k_quo_C["5"] )
<A row module over Q of rank 50>

julia> Dimension( quo_k_quo_C )
126

julia> ModelingCategory( quo_k_quo_C )
Q-LinearClosure( PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,
c:1→3, e:1→4,b:2→4,d:3→4,t:4→5,y:5→5]" ) ) ) / [ 1*x^10 + (-1)*x^5,
1*a⋅b⋅t + (-1)*e⋅t, 1*y^10 + (-1)*y^5, ... ]

julia> B = AlgebroidFromDataTables( quo_k_quo_C )
Q-algebroid( [0,1,2,3,4,5][x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,b:2→4,d:3→4,
t:4→5,y:5→5] ) defined by 6 objects and 9 generating morphisms

julia> HomStructure( B["0"], B["5"] )
<A row module over Q of rank 50>

```

```jldoctest AutoDocTests
julia> using MatricesForHomalg, CAP, MonoidalCategories, CartesianCategories, ToolsForCategoricalTowers, Toposes, FinSetsForCAP, QuotientCategories, FpCategories, LinearClosuresForCAP, AdditiveClosuresForCAP, LinearAlgebraForCAP, FreydCategoriesForCAP, FpLinearCategories

julia> true
true

julia> k = HomalgFieldOfRationals( );

julia> q = FinQuiver( "q(o)[x:o->o,y:o->o]" )
FinQuiver( "q(o)[x:o→o,y:o→o]" )

julia> C = PathCategory( q )
PathCategory( FinQuiver( "q(o)[x:o→o,y:o→o]" ) )

julia> kC = k[C]
Q-LinearClosure( PathCategory( FinQuiver( "q(o)[x:o→o,y:o→o]" ) ) )

julia> x = kC.x
1*x:(o) → (o)

julia> y = kC.y
1*y:(o) → (o)

julia> rels = [ x*y-y*x, (x^2+y^2)*(x+x*y), (x^2+y^2)*(y^2+x^3) ];

julia> Perform( rels, Display )
(-1)*y⋅x + 1*x⋅y:(o) → (o)
1*x^3⋅y + 1*y^2⋅x⋅y + 1*x^3 + 1*y^2⋅x:(o) → (o)
1*x^5 + 1*y^2⋅x^3 + 1*x^2⋅y^2 + 1*y^4:(o) → (o)

julia> gb = ReducedGroebnerBasis( kC, rels );

julia> Perform( gb, Display )
(-1)*y⋅x + 1*x⋅y:(o) → (o)
1*x^3⋅y + 1*x⋅y^3 + 1*x^3 + 1*x⋅y^2:(o) → (o)
1*x^5 + (-1)*x⋅y^4 + 1*x^2⋅y^2 + 1*y^4 + 1*x^3 + 1*x⋅y^2:(o) → (o)
1*x^2⋅y^3 + 1*y^5 + 1*x^2⋅y^2 + 1*y^4:(o) → (o)

julia> f = (x-y)*gb[1] + (x^2-y)*gb[2] + y^3*gb[3] + (x-y^3)*gb[4]
1*y^3⋅x^5 + (-1)*y^3⋅x^2⋅y^3 + (-1)*y^3⋅x⋅y^4 + (-1)*y^8 + 1*y^3⋅x^3
+ 1*x^5⋅y + 1*y^3⋅x⋅y^2 + 2*x^3⋅y^3 + 1*x⋅y^5 + 1*x^5 + (-1)*y⋅x^3⋅y
+ 2*x^3⋅y^2 + (-1)*y⋅x⋅y^3 + 1*x⋅y^4 + (-1)*y⋅x^3 + (-1)*y⋅x⋅y^2
+ (-1)*x⋅y⋅x + 1*y^2⋅x + 1*x^2⋅y + (-1)*y⋅x⋅y:(o) → (o)

julia> ReductionOfMorphism( kC, f, rels )
(-1)*x^2⋅y^6 + (-1)*y^8 + 1*x^2⋅y^2 + 1*y^4:(o) → (o)

julia> ReductionOfMorphism( kC, f, gb )
0:(o) → (o)

```
