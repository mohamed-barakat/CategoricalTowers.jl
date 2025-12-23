# SPDX-License-Identifier: GPL-2.0-or-later
# FpCategories: Finitely presented categories by generating quivers and relations
#
# Reading the implementation part of the package.
#

include( "gap/precompiled_categories/CategoryFromNerveDataPrecompiled.gi.autogen.jl" );
include( "gap/precompiled_categories/CategoryFromNerveDataHomStructureOnMorphismsPrecompiled.gi.autogen.jl" );

include( "gap/Quivers.gi.autogen.jl");
include( "gap/PathCategories.gi.autogen.jl");
include( "gap/GroebnerBasesForPathCategories.gi.autogen.jl");
include( "gap/QuotientsOfPathCategories.gi.autogen.jl");
include( "gap/CategoryFromNerveData.gi.autogen.jl");
include( "gap/Tools.gi.autogen.jl");
include( "gap/ToolsDerivedMethods.gi.autogen.jl");
include( "gap/SimplicialCategory.gi.autogen.jl");

#= comment for Julia
if IsPackageMarkedForLoading( "Digraphs", ">= 1.3.1" ) then
    include( "gap/ToolsUsingDigraphs.gi.autogen.jl");
fi;

if IsPackageMarkedForLoading( "JuliaInterface", ">= 0.2" ) then
    include( "gap/Julia.gi.autogen.jl" );
fi;
# =#
