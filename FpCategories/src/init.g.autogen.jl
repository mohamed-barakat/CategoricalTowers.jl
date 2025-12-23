# SPDX-License-Identifier: GPL-2.0-or-later
# FpCategories: Finitely presented categories by generating quivers and relations
#
# Reading the declaration part of the package.
#

include( "gap/Quivers.gd.autogen.jl");
include( "gap/PathCategories.gd.autogen.jl");
include( "gap/GroebnerBasesForPathCategories.gd.autogen.jl");
include( "gap/QuotientsOfPathCategories.gd.autogen.jl");
include( "gap/CategoryFromNerveData.gd.autogen.jl");
include( "gap/Tools.gd.autogen.jl");
include( "gap/SimplicialCategory.gd.autogen.jl");

#= comment for Julia
if IsPackageMarkedForLoading( "Digraphs", ">= 1.3.1" ) then
    include( "gap/ToolsUsingDigraphs.gd.autogen.jl");
fi;
# =#
