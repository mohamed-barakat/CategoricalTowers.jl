
```jldoctest AutoDocTests
julia> using CAP, QuotientCategories, FpCategories

julia> true
true

julia> str = "q(0..5)[x:0->0,s:0->1,a:1->2,c:1->3,e:1->4,b:2->4,d:3->4,t:4->5,y:5->5]";

julia> q = FinQuiver( str )
FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,b:2→4,d:3→4,
t:4→5,y:5→5]" )

julia> QuiverName( q )
"q"

julia> NumberOfObjects( q )
6

julia> Display( LabelsOfObjects( q ) )
[ "0", "1", "2", "3", "4", "5" ]

julia> Display( SetOfObjects( q ) )
[ (0), (1), (2), (3), (4), (5) ]

julia> o = q["0"]
(0)

julia> ObjectIndex( o )
1

julia> IsIdenticalObj( o, ObjectConstructor( q, 1 ) )
true

julia> ObjectLabel( o )
"0"

julia> LaTeXOutput( o )
"0"

julia> NumberOfMorphisms( q )
9

julia> Display( LabelsOfMorphisms( q ) )
[ "x", "s", "a", "c", "e", "b", "d", "t", "y" ]

julia> Display( IndicesOfSources( q ) )
[ 1, 1, 2, 2, 2, 3, 4, 5, 6 ]

julia> Display( IndicesOfTargets( q ) )
[ 1, 2, 3, 4, 5, 5, 5, 6, 6 ]

julia> Display( SetOfMorphisms( q ) )
[ x:(0) → (0), s:(0) → (1), a:(1) → (2), c:(1) → (3), e:(1) → (4),
  b:(2) → (4), d:(3) → (4), t:(4) → (5), y:(5) → (5) ]

julia> m = q.y
y:(5) → (5)

julia> MorphismIndex( m )
9

julia> IsIdenticalObj( m, MorphismConstructor( q, q["5"], 9, q["5"] ) )
true

julia> MorphismLabel( m )
"y"

julia> Display( MorphismsOfExternalHom( q["0"], q["0"] ) )
[ x:(0) → (0) ]

julia> Display( MorphismsOfExternalHom( q["0"], q["1"] ) )
[ s:(0) → (1) ]

julia> q_op = OppositeQuiver( q )
FinQuiver( "q_op(0,1,2,3,4,5)[x:0→0,s:1→0,a:2→1,c:3→1,e:4→1,b:4→2,
d:4→3,t:5→4,y:5→5]" )

julia> Display( MorphismsOfExternalHom( q_op["1"], q_op["0"] ) )
[ s:(1) → (0) ]

julia> q_op_x_q = TensorProductOfFinQuivers( q_op, q );

julia> NumberOfObjects( q_op_x_q )
36

julia> NumberOfMorphisms( q_op_x_q )
108

```

```jldoctest AutoDocTests
julia> using CAP, QuotientCategories, FpCategories

julia> q1 = RandomFinQuiver( 2, 4, false );

julia> q2 = RandomFinQuiver( 2, 4, true );

julia> @Assert( 0, NumberOfObjects( q1 ) == 2 )

julia> @Assert( 0, NumberOfMorphisms( q1 ) == 4 )

julia> @Assert( 0, Length( Filtered( SetOfMorphisms( q1 ), IsEndomorphism ) ) == 0 )

julia> @Assert( 0, Length( Filtered( SetOfMorphisms( q2 ), IsEndomorphism ) ) > 0 )

julia> q3 = FinQuiver( "q(3)[a:1->2,b:2->3]" )
FinQuiver( "q(1,2,3)[a:1→2,b:2→3]" )

julia> q4 = FinQuiver( "q(4..6)[a:4->5,b:5->6]" )
FinQuiver( "q(4,5,6)[a:4→5,b:5→6]" )

```

```jldoctest AutoDocTests
julia> using CAP, QuotientCategories, FpCategories

julia> true
true

julia> Delta2 = SimplicialCategoryTruncatedInDegree( 2 )
PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == id(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ]

julia> Perform( DefiningRelations( Delta2 ), Display )
[ s⋅id:(C0) → (C0), id(C0):(C0) → (C0) ]
[ t⋅id:(C0) → (C0), id(C0):(C0) → (C0) ]
[ ps⋅is:(C1) → (C1), id(C1):(C1) → (C1) ]
[ pt⋅it:(C1) → (C1), id(C1):(C1) → (C1) ]
[ is⋅id:(C2) → (C0), it⋅id:(C2) → (C0) ]
[ pt⋅is:(C1) → (C1), id⋅t:(C1) → (C1) ]
[ ps⋅it:(C1) → (C1), id⋅s:(C1) → (C1) ]
[ s⋅pt:(C0) → (C2), t⋅ps:(C0) → (C2) ]
[ s⋅mu:(C0) → (C2), s⋅ps:(C0) → (C2) ]
[ t⋅mu:(C0) → (C2), t⋅pt:(C0) → (C2) ]
[ mu⋅is:(C1) → (C1), id(C1):(C1) → (C1) ]
[ mu⋅it:(C1) → (C1), id(C1):(C1) → (C1) ]

julia> Size( Delta2 )
31

julia> N = NerveTruncatedInDegree2AsFunctor( Delta2 )
Functor from PathCategory( FinQuiver(
  "Delta_op(C0,C1,C2)[id:C0→C1,s:C1→C0,t:C1→C0,
                      is:C1→C2,it:C1→C2,ps:C2→C1,pt:C2→C1,mu:C2→C1]" ) )
/ [ id⋅s == id(C0), id⋅t == id(C0), is⋅ps == id(C1), ... ] -> SkeletalFinSets

julia> Delta2op = SourceOfFunctor( N )
PathCategory( FinQuiver(
  "Delta_op(C0,C1,C2)[id:C0→C1,s:C1→C0,t:C1→C0,
                      is:C1→C2,it:C1→C2,ps:C2→C1,pt:C2→C1,mu:C2→C1]" ) )
/ [ id⋅s == id(C0), id⋅t == id(C0), is⋅ps == id(C1), ... ]

julia> ApplyFunctor( N, Delta2op.C0 )
|3|

julia> Display( ApplyFunctor( N, Delta2op.C0 ) )
[ 0, 1, 2 ]

julia> ApplyFunctor( N, Delta2op.C1 )
|31|

julia> Display( ApplyFunctor( N, Delta2op.C1 ) )
[ 0,..., 30 ]

julia> ApplyFunctor( N, Delta2op.C2 )
|393|

julia> Display( ApplyFunctor( N, Delta2op.C2 ) )
[ 0,..., 392 ]

julia> ApplyFunctor( N, Delta2op.id )
|3| → |31|

julia> Display( ApplyFunctor( N, Delta2op.id ) )
[ 0, 1, 2 ] ⱶ[ 0, 5, 21 ]→ [ 0,..., 30 ]

```

```jldoctest AutoDocTests
julia> using CAP, QuotientCategories, FpCategories

julia> Delta2 = SimplicialCategoryTruncatedInDegree( 2 )
PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == id(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ]

julia> Perform( DefiningRelations( Delta2 ), Display )
[ s⋅id:(C0) → (C0), id(C0):(C0) → (C0) ]
[ t⋅id:(C0) → (C0), id(C0):(C0) → (C0) ]
[ ps⋅is:(C1) → (C1), id(C1):(C1) → (C1) ]
[ pt⋅it:(C1) → (C1), id(C1):(C1) → (C1) ]
[ is⋅id:(C2) → (C0), it⋅id:(C2) → (C0) ]
[ pt⋅is:(C1) → (C1), id⋅t:(C1) → (C1) ]
[ ps⋅it:(C1) → (C1), id⋅s:(C1) → (C1) ]
[ s⋅pt:(C0) → (C2), t⋅ps:(C0) → (C2) ]
[ s⋅mu:(C0) → (C2), s⋅ps:(C0) → (C2) ]
[ t⋅mu:(C0) → (C2), t⋅pt:(C0) → (C2) ]
[ mu⋅is:(C1) → (C1), id(C1):(C1) → (C1) ]
[ mu⋅it:(C1) → (C1), id(C1):(C1) → (C1) ]

julia> Size( Delta2 )
31

julia> Ymu = YonedaCompositionAsNaturalEpimorphism( Delta2 )
Natural transformation from
Functor from PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == i d(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ] -> SkeletalFinSets
->
Functor from PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == i d(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ] -> SkeletalFinSets

julia> Ymu = YonedaProjectionAsNaturalEpimorphism( Delta2 )
Natural transformation from
Functor from PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == i d(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ] -> SkeletalFinSets
->
Functor from PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == i d(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ] -> SkeletalFinSets

julia> Ys = YonedaFibrationAsNaturalTransformation( Delta2 )
Natural transformation from
Functor from PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == i d(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ] -> SkeletalFinSets
->
Functor from PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == i d(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ] -> SkeletalFinSets

```

```jldoctest AutoDocTests
julia> using CAP, QuotientCategories, FpCategories

julia> true
true

julia> Delta1 = SimplicialCategoryTruncatedInDegree( 1 )
PathCategory( FinQuiver( "Delta(C0,C1)[id:C1→C0,s:C0→C1,t:C0→C1]" ) )
/ [ s⋅id == id(C0), t⋅id == id(C0) ]

julia> Size( Delta1 )
7

julia> C = CategoryFromNerveData( Delta1 )
PathCategory( FinQuiver( "Delta(C0,C1)[id:C1→C0,s:C0→C1,t:C0→C1]" ) )
/ [ s⋅id == id(C0), t⋅id == id(C0) ]

julia> Size( C )
7

julia> NerveTruncatedInDegree2Data( C ) == NerveTruncatedInDegree2Data( Delta1 )
true

julia> Display( IndicesOfGeneratingMorphisms( C ) )
[ 1, 2, 3 ]

julia> Perform( SetOfGeneratingMorphisms( C ), Display )
(C1)-[(id)]→(C0)
(C0)-[(s)]→(C1)
(C0)-[(t)]→(C1)

julia> Display( C )
A CAP category with name
PathCategory( FinQuiver( "Delta(C0,C1)[id:C1→C0,s:C0→C1,t:C0→C1]" ) )
/ [ s⋅id == id(C0), t⋅id == id(C0) ]:

19 primitive operations were used to derive 55 operations for this category
which algorithmically
* IsCategoryWithDecidableColifts
* IsCategoryWithDecidableLifts
* IsFiniteCategory
* IsEquippedWithHomomorphismStructure

julia> C0 = CreateObject( C, 0 )
<(C0)>

julia> IsWellDefined( C0 )
true

julia> C1 = 1 / C
<(C1)>

julia> IsWellDefined( C1 )
true

julia> IsWellDefined( 2 / C )
false

julia> idC0 = CreateMorphism( C0, 0, C0 )
(C0)-[(C0)]→(C0)

julia> CreateMorphism( C, 0 ) == idC0
true

julia> IsOne( idC0 )
true

julia> id = CreateMorphism( C, 1 )
(C1)-[(id)]→(C0)

julia> s = CreateMorphism( C, 2 )
(C0)-[(s)]→(C1)

julia> t = CreateMorphism( C, 3 )
(C0)-[(t)]→(C1)

julia> idC1 = CreateMorphism( C, 4 )
(C1)-[(C1)]→(C1)

julia> IsOne( idC1 )
true

julia> sigma = CreateMorphism( C, 5 )
(C1)-[(id⋅s)]→(C1)

julia> IsEndomorphism( sigma )
true

julia> IsOne( sigma )
false

julia> tau = CreateMorphism( C, 6 )
(C1)-[(id⋅t)]→(C1)

julia> IsEndomorphism( tau )
true

julia> IsOne( tau )
false

julia> IsWellDefined( CreateMorphism( C1, 7, C1 ) )
false

julia> PreCompose( s, id ) == idC0
true

julia> PreCompose( t, id ) == idC0
true

julia> PreCompose( id, s ) == sigma
true

julia> PreCompose( id, t ) == tau
true

julia> HomStructure( C0, C0 )
|1|

julia> HomStructure( C1, C1 )
|3|

julia> HomStructure( C0, C1 )
|2|

julia> HomStructure( C1, C0 )
|1|

julia> Display( HomStructure( s ) )
[ 0 ] ⱶ[ 0 ]→ [ 0, 1 ]

julia> Display( HomStructure( t ) )
[ 0 ] ⱶ[ 1 ]→ [ 0, 1 ]

julia> HomStructure( Source( s ), Target( s ), HomStructure( s ) ) == s
true

julia> HomStructure( Source( t ), Target( t ), HomStructure( t ) ) == t
true

julia> Display( HomStructure( s, t ) )
[ 0 ] ⱶ[ 1 ]→ [ 0, 1 ]

julia> Display( HomStructure( t, s ) )
[ 0 ] ⱶ[ 0 ]→ [ 0, 1 ]

julia> Display( HomStructure( sigma, tau ) )
[ 0, 1, 2 ] ⱶ[ 2, 2, 2 ]→ [ 0, 1, 2 ]

julia> Display( HomStructure(
                PreCompose( Delta1.id, Delta1.s ),
                PreCompose( Delta1.id, Delta1.t ) ) )
[ 0, 1, 2 ] ⱶ[ 2, 2, 2 ]→ [ 0, 1, 2 ]

julia> Display( HomStructure( tau, sigma ) )
[ 0, 1, 2 ] ⱶ[ 1, 1, 1 ]→ [ 0, 1, 2 ]

julia> Display( HomStructure(
                PreCompose( Delta1.id, Delta1.t ),
                PreCompose( Delta1.id, Delta1.s ) ) )
[ 0, 1, 2 ] ⱶ[ 1, 1, 1 ]→ [ 0, 1, 2 ]

julia> Display( HomStructure( tau, idC1 ) )
[ 0, 1, 2 ] ⱶ[ 2, 1, 2 ]→ [ 0, 1, 2 ]

julia> Display( HomStructure( idC1, idC1 ) )
[ 0, 1, 2 ] ⱶ[ 0, 1, 2 ]→ [ 0, 1, 2 ]

julia> C_op = OppositeCategoryFromNerveData( C )
Opposite(
PathCategory( FinQuiver( "Delta(C0,C1)[id:C1→C0,s:C0→C1,t:C0→C1]" ) )
/ [ s⋅id == id(C0), t⋅id == id(C0) ] )

julia> NerveData( C_op ) == NerveTruncatedInDegree2Data( C_op )
true

julia> IsIdenticalObj( OppositeCategoryFromNerveData( C_op ), C )
true

julia> Display( IndicesOfGeneratingMorphisms( C_op ) )
[ 3, 1, 2 ]

julia> Perform( SetOfGeneratingMorphisms( C_op ), Display )
(C0)-[(id)]→(C1)
(C1)-[(s)]→(C0)
(C1)-[(t)]→(C0)

```

```jldoctest AutoDocTests
julia> using CAP, QuotientCategories, FpCategories

julia> true
true

julia> str = "q(0..5)[x:0->0,s:0->1,a:1->2,c:1->3,e:1->4,b:2->4,d:3->4,t:4->5,y:5->5]";

julia> q = FinQuiver( str )
FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,b:2→4,d:3→4,
t:4→5,y:5→5]" )

julia> C = PathCategory( q; admissible_order = "Dp" )
PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,
b:2→4,d:3→4,t:4→5,y:5→5]" ) )

julia> Display( C )
A CAP category with name PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,
a:1→2,c:1→3,e:1→4,b:2→4,d:3→4,t:4→5,y:5→5]" ) ):

17 primitive operations were used to derive 32 operations for this category
which algorithmically
* IsFinitelyPresentedCategory

julia> Display( SetOfObjects( C ) )
[ (0), (1), (2), (3), (4), (5) ]

julia> Display( SetOfGeneratingMorphisms( C ) )
[ x:(0) → (0), s:(0) → (1), a:(1) → (2), c:(1) → (3), e:(1) → (4),
  b:(2) → (4), d:(3) → (4), t:(4) → (5), y:(5) → (5) ]

julia> C["5"]
(5)

julia> ObjectIndex( C["5"] )
6

julia> C.id_5 == "id(5)" / C
true

julia> m = C["x^2*s*a*b*t*y^2"]
x^2⋅s⋅a⋅b⋅t⋅y^2:(0) → (5)

julia> m = C["x^2sabty^2"]
x^2⋅s⋅a⋅b⋅t⋅y^2:(0) → (5)

julia> IsWellDefined( m )
true

julia> MorphismLength( m )
8

julia> Display( MorphismIndices( m ) )
[ 1, 1, 2, 3, 6, 8, 9, 9 ]

julia> Perform( MorphismSupport( m ), Display )
x:(0) → (0)
x:(0) → (0)
s:(0) → (1)
a:(1) → (2)
b:(2) → (4)
t:(4) → (5)
y:(5) → (5)
y:(5) → (5)

julia> m == MorphismConstructor( C, Source( m ), [ MorphismLength( m ), MorphismIndices( m ) ], Target( m ) )
true

julia> relations = [ [ C.x^5, C.x ], [ C.y^5, C.y^2 ] ];

julia> qC = QuotientCategory( C, relations )
PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,a:1→2,c:1→3,e:1→4,
b:2→4,d:3→4,t:4→5,y:5→5]" ) ) / [ x^5 == x, y^5 == y^2 ]

julia> Display( qC )
A CAP category with name PathCategory( FinQuiver( "q(0,1,2,3,4,5)[x:0→0,s:0→1,
a:1→2,c:1→3,e:1→4,b:2→4,d:3→4,t:4→5,y:5→5]" ) ) / [ x^5 == x, y^5 == y^2 ]:

24 primitive operations were used to derive 65 operations for this category
which algorithmically
* IsCategoryWithDecidableColifts
* IsCategoryWithDecidableLifts
* IsFiniteCategory
* IsEquippedWithHomomorphismStructure

julia> "0" / qC
(0)

julia> ObjectIndex( qC["0"] )
1

julia> qC.x^7
[x^3]:(0) → (0)

julia> CanonicalRepresentative( qC.x^7 )
x^3:(0) → (0)

julia> HomomorphismStructureOnObjects( qC["0"], qC["5"] )
|75|

julia> Display( List( SetOfGeneratingMorphisms( qC ), IsMonomorphism ) )
[ false, true, true, true, true, true, true, true, false ]

julia> Display( List( SetOfGeneratingMorphisms( qC ), IsEpimorphism ) )
[ false, true, true, true, true, true, true, true, false ]

```

```jldoctest AutoDocTests
julia> using CAP, QuotientCategories, FpCategories

julia> true
true

julia> Delta2 = SimplicialCategoryTruncatedInDegree( 2 )
PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) )
/ [ s⋅id == id(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ]

julia> Perform( DefiningRelations( Delta2 ), Display )
[ s⋅id:(C0) → (C0), id(C0):(C0) → (C0) ]
[ t⋅id:(C0) → (C0), id(C0):(C0) → (C0) ]
[ ps⋅is:(C1) → (C1), id(C1):(C1) → (C1) ]
[ pt⋅it:(C1) → (C1), id(C1):(C1) → (C1) ]
[ is⋅id:(C2) → (C0), it⋅id:(C2) → (C0) ]
[ pt⋅is:(C1) → (C1), id⋅t:(C1) → (C1) ]
[ ps⋅it:(C1) → (C1), id⋅s:(C1) → (C1) ]
[ s⋅pt:(C0) → (C2), t⋅ps:(C0) → (C2) ]
[ s⋅mu:(C0) → (C2), s⋅ps:(C0) → (C2) ]
[ t⋅mu:(C0) → (C2), t⋅pt:(C0) → (C2) ]
[ mu⋅is:(C1) → (C1), id(C1):(C1) → (C1) ]
[ mu⋅it:(C1) → (C1), id(C1):(C1) → (C1) ]

julia> Size( Delta2 )
31

julia> Delta2_op = OppositeFiniteCategory( Delta2 )
Opposite( PathCategory( FinQuiver(
  "Delta(C0,C1,C2)[id:C1→C0,s:C0→C1,t:C0→C1,
                   is:C2→C1,it:C2→C1,
                   ps:C1→C2,pt:C1→C2,mu:C1→C2]" ) ) /
[ s⋅id == id(C0), t⋅id == id(C0), ps⋅is == id(C1), ... ] )

julia> IsIdenticalObj( OppositeFiniteCategory( Delta2_op ), Delta2 )
true

```
