#!/bin/bash

set -e

# Check if token exists
if [ -z "$JULIA_SUBSPLIT_TOKEN" ]; then
  echo -e "\033[31mERROR: Missing JULIA_SUBSPLIT_TOKEN\033[0m"
  exit 1
fi

# update git index
git status > /dev/null

if ! git diff-index --quiet HEAD; then
	echo -e "\033[33mWARNING: Dirty working tree.\033[0m"
fi

if [[ "$(git rev-parse --abbrev-ref HEAD)" != "master" ]]; then
	echo -e "\033[31mERROR: Not on branch master\033[0m"
	exit 1
fi

# ToolsForCategoricalTowers
git subtree split --prefix=ToolsForCategoricalTowers -b ToolsForCategoricalTowers-split > /dev/null 2>&1
git push https://${JULIA_SUBSPLIT_TOKEN}@github.com/homalg-project/ToolsForCategoricalTowers.jl.git ToolsForCategoricalTowers-split:master
echo "Pushed to ToolsForCategoricalTowers.jl"

# QuotientCategories
git subtree split --prefix=QuotientCategories -b QuotientCategories-split > /dev/null 2>&1
git push https://${JULIA_SUBSPLIT_TOKEN}@github.com/homalg-project/QuotientCategories.jl.git QuotientCategories-split:master
echo "Pushed to QuotientCategories.jl"

# FpCategories
git subtree split --prefix=FpCategories -b FpCategories-split > /dev/null 2>&1
git push https://${JULIA_SUBSPLIT_TOKEN}@github.com/homalg-project/FpCategories.jl.git FpCategories-split:master
echo "Pushed to FpCategories.jl"
